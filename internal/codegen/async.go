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
