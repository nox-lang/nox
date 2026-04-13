package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

// emitAsyncFunc generates the pieces needed to make an `async func` (or an
// async instance/static method, when thisType is non-nil) callable and
// awaitable, on top of the shared nox_task runtime (lib/nox_thread.c):
//
//  1. a plain synchronous "body" function holding the real logic
//  2. a per-instantiation argument-bundle struct (nox_task_start's body
//     signature only takes one void* argument)
//  3. a trampoline matching nox_body_fn's signature, which unpacks the
//     bundle, runs the body, and stores the result through its `result`
//     out-parameter
//  4. the public entry point (what Nox call sites actually invoke), which
//     allocates the bundle and calls nox_task_start, returning the
//     resulting nox_task* immediately (ctype(KTask) == "nox_task*")
func (cg *Codegen) emitAsyncFunc(fi *FuncInstance, decl *ast.FuncDecl, argTypes []Type, thisType *Type) {
	bodyName := fi.MangledName + "__body"
	threadName := fi.MangledName + "__thread"
	argsStructName := fi.MangledName + "__args"
	fi.AsyncBodyName = bodyName
	fi.AsyncThreadName = threadName

	scope := newScope(nil)
	if thisType != nil {
		scope.define("this", *thisType)
	}
	for i, p := range decl.Params {
		scope.define(p.Name, argTypes[i])
	}
	fb := &funcBuilder{cg: cg, fname: bodyName, isAsync: true, selfInstance: fi}
	if fi.RetTypeKnown {
		fb.retType = fi.RetType
		fb.retTypeKnown = true
	}
	bodyC := fb.buildFunctionBody(scope, decl.Body, "")
	fi.RetType = fb.retType
	fi.RetTypeKnown = true
	resultC := cg.ctype(fi.RetType)
	taskC := cg.ctype(TTask(fi.RetType)) // "nox_task*"

	// paramList builds "(TYPE name, ...)" including an optional leading
	// `this` receiver, shared by the body/bundle/public-entry pieces below.
	paramList := func() []string {
		var ps []string
		if thisType != nil {
			ps = append(ps, fmt.Sprintf("%s %s", cg.ctype(*thisType), cIdent("this")))
		}
		for i, p := range decl.Params {
			ps = append(ps, fmt.Sprintf("%s %s", cg.ctype(argTypes[i]), cIdent(p.Name)))
		}
		if len(ps) == 0 {
			ps = append(ps, "void")
		}
		return ps
	}

	bodyRetC := "void"
	if fi.RetType.Kind != KVoid {
		bodyRetC = resultC
	}

	var sbFwd, sbDef strings.Builder

	bodyParams := paramList()
	sbFwd.WriteString(fmt.Sprintf("static %s %s(%s);\n", bodyRetC, bodyName, strings.Join(bodyParams, ", ")))
	sbDef.WriteString(fmt.Sprintf("static %s %s(%s) {\n%s}\n\n", bodyRetC, bodyName, strings.Join(bodyParams, ", "), indent(bodyC, "    ")))

	// Argument bundle struct (typedef so the trampoline's lone void* can be
	// cast back into a real, typed pointer).
	var argFields strings.Builder
	if thisType != nil {
		argFields.WriteString(fmt.Sprintf("    %s %s;\n", cg.ctype(*thisType), cIdent("this")))
	}
	for i, p := range decl.Params {
		argFields.WriteString(fmt.Sprintf("    %s %s;\n", cg.ctype(argTypes[i]), cIdent(p.Name)))
	}
	if argFields.Len() == 0 {
		argFields.WriteString("    char __unused;\n")
	}
	sbDef.WriteString(fmt.Sprintf("typedef struct {\n%s} %s;\n\n", argFields.String(), argsStructName))

	// Trampoline: matches nox_body_fn (void(*)(void *arg, void *result)).
	sbFwd.WriteString(fmt.Sprintf("static void %s(void *__raw, void *__result);\n", threadName))
	var callArgs []string
	if thisType != nil {
		callArgs = append(callArgs, fmt.Sprintf("__a->%s", cIdent("this")))
	}
	for _, p := range decl.Params {
		callArgs = append(callArgs, fmt.Sprintf("__a->%s", cIdent(p.Name)))
	}
	var trampolineBody strings.Builder
	trampolineBody.WriteString(fmt.Sprintf("%s* __a = (%s*)__raw;\n", argsStructName, argsStructName))
	if fi.RetType.Kind != KVoid {
		trampolineBody.WriteString(fmt.Sprintf("%s __r = %s(%s);\n", resultC, bodyName, strings.Join(callArgs, ", ")))
		trampolineBody.WriteString(fmt.Sprintf("if (__result) { *(%s*)__result = __r; }\n", resultC))
	} else {
		trampolineBody.WriteString(fmt.Sprintf("%s(%s);\n", bodyName, strings.Join(callArgs, ", ")))
	}
	sbDef.WriteString(fmt.Sprintf("static void %s(void *__raw, void *__result) {\n%s}\n\n", threadName, indent(trampolineBody.String(), "    ")))

	// Public entry point.
	pubParams := paramList()
	sbFwd.WriteString(fmt.Sprintf("static %s %s(%s);\n", taskC, fi.MangledName, strings.Join(pubParams, ", ")))

	var pubBody strings.Builder
	pubBody.WriteString(fmt.Sprintf("%s* __argp = (%s*)NOX_ALLOC(sizeof(%s));\n", argsStructName, argsStructName, argsStructName))
	if thisType != nil {
		pubBody.WriteString(fmt.Sprintf("__argp->%s = %s;\n", cIdent("this"), cIdent("this")))
	}
	for _, p := range decl.Params {
		pubBody.WriteString(fmt.Sprintf("__argp->%s = %s;\n", cIdent(p.Name), cIdent(p.Name)))
	}
	resultSize := "0"
	if fi.RetType.Kind != KVoid {
		resultSize = fmt.Sprintf("(int64_t)sizeof(%s)", resultC)
	}
	pubBody.WriteString(fmt.Sprintf("return nox_task_start(%s, __argp, %s);\n", threadName, resultSize))
	sbDef.WriteString(fmt.Sprintf("static %s %s(%s) {\n%s}\n", taskC, fi.MangledName, strings.Join(pubParams, ", "), indent(pubBody.String(), "    ")))

	fi.Forward = sbFwd.String()
	fi.Body = sbDef.String()
}

// ---------------- Thread / Task built-ins ----------------
//
// `Thread.new(fn)`, `Task.Run(fn)` and `Task.WhenAll(tasks)` are a second,
// explicit way to reach the same nox_task/nox_thread runtime that `async
// func`/`await`/`parallel` uses above — modeled on C#'s Task API. `fn` must
// be a zero-argument anonymous function; its return type (if any) becomes
// the Task's result type. Both families freely interoperate: a value
// produced by Task.Run(...) can be `await`-ed, and an `async func` call can
// be placed in a []Task and passed to Task.WhenAll(...).

// ensureZeroArgTrampoline returns (creating it once, if necessary) a
// nox_body_fn-shaped trampoline that invokes a zero-argument closure
// (NoxFn_<ret>, no params) stored at the arg pointer, storing its result (if
// any) through the result pointer. Shared by every Thread.new/Task.Run call
// site whose closure has the same return type — the closure's captured
// environment is already opaque (void*) inside the NoxFn_* struct, so one
// trampoline per return type covers every capture shape.
func (cg *Codegen) ensureZeroArgTrampoline(retType Type) string {
	key := "void"
	if retType.Kind != KVoid {
		key = mangle(retType)
	}
	if name, ok := cg.zeroArgTrampolines[key]; ok {
		return name
	}
	closureType := Type{Kind: KFunc}
	if retType.Kind != KVoid {
		r := retType
		closureType.Ret = &r
	}
	cloC := cg.ctype(closureType) // registers NoxFn_<ret>

