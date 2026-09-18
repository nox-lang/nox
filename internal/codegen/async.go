package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

// emitAsyncFunc generates the four pieces needed to make an `async func`
// (or async method, when thisType is non-nil) callable and awaitable:
//
//  1. a plain synchronous "body" function holding the real logic
//  2. a per-instantiation argument-bundle struct (a pthread start routine
//     only takes one void* argument)
//  3. a pthread trampoline that unpacks the bundle, runs the body, and
//     stores the result
//  4. the public entry point (what Nox call sites actually invoke), which
//     allocates the bundle, spawns the thread, and immediately returns a
//     NoxTask_T handle
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
	taskC := cg.ctype(TTask(fi.RetType)) // registers the NoxTask_<T> typedef

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

	// Argument bundle struct (typedef so pthread's start routine can cast
	// its lone void* back into a real, typed pointer).
	var argFields strings.Builder
	if thisType != nil {
		argFields.WriteString(fmt.Sprintf("    %s %s;\n", cg.ctype(*thisType), cIdent("this")))
	}
	for i, p := range decl.Params {
		argFields.WriteString(fmt.Sprintf("    %s %s;\n", cg.ctype(argTypes[i]), cIdent(p.Name)))
	}
	argFields.WriteString(fmt.Sprintf("    %s* __result;\n", resultC))
	// (the struct typedef itself is emitted directly into the body block,
	// right before its first use, since nothing in the forward-declarations
	// block above needs to name this type)
	sbDef.WriteString(fmt.Sprintf("typedef struct {\n%s} %s;\n\n", argFields.String(), argsStructName))

	// Trampoline.
	sbFwd.WriteString(fmt.Sprintf("static NOX_THREAD_FUNC %s(nox_thread_arg_t __raw);\n", threadName))
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
		trampolineBody.WriteString(fmt.Sprintf("*(__a->__result) = %s(%s);\n", bodyName, strings.Join(callArgs, ", ")))
	} else {
		trampolineBody.WriteString(fmt.Sprintf("%s(%s);\n", bodyName, strings.Join(callArgs, ", ")))
	}
	trampolineBody.WriteString("NOX_THREAD_RETURN;\n")
	sbDef.WriteString(fmt.Sprintf("static NOX_THREAD_FUNC %s(nox_thread_arg_t __raw) {\n%s}\n\n", threadName, indent(trampolineBody.String(), "    ")))

	// Public entry point.
	pubParams := paramList()
	sbFwd.WriteString(fmt.Sprintf("static %s %s(%s);\n", taskC, fi.MangledName, strings.Join(pubParams, ", ")))

	var pubBody strings.Builder
	pubBody.WriteString(fmt.Sprintf("%s* __argp = (%s*)GC_MALLOC(sizeof(%s));\n", argsStructName, argsStructName, argsStructName))
	if thisType != nil {
		pubBody.WriteString(fmt.Sprintf("__argp->%s = %s;\n", cIdent("this"), cIdent("this")))
	}
	for _, p := range decl.Params {
		pubBody.WriteString(fmt.Sprintf("__argp->%s = %s;\n", cIdent(p.Name), cIdent(p.Name)))
	}
	if fi.RetType.Kind != KVoid {
		pubBody.WriteString(fmt.Sprintf("__argp->__result = (%s*)GC_MALLOC(sizeof(%s));\n", resultC, resultC))
	} else {
		pubBody.WriteString("__argp->__result = NULL;\n")
	}
	pubBody.WriteString(fmt.Sprintf("%s __t;\n", taskC))
	pubBody.WriteString("__t.result = __argp->__result;\n")
	pubBody.WriteString(fmt.Sprintf("NOX_THREAD_CREATE(&__t.th, %s, __argp);\n", threadName))
	pubBody.WriteString("return __t;\n")
	sbDef.WriteString(fmt.Sprintf("static %s %s(%s) {\n%s}\n", taskC, fi.MangledName, strings.Join(pubParams, ", "), indent(pubBody.String(), "    ")))

	fi.Forward = sbFwd.String()
	fi.Body = sbDef.String()
}
