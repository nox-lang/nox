package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

// resolveCallArgs evaluates a call's argument expressions against a target
// parameter list, filling in default values and collecting a trailing
// variadic parameter into an array. It returns, for each *logical* parameter
// (variadic collapses to exactly one slot), the generated C code and Type.
func (fb *funcBuilder) resolveCallArgs(c *ctx, fname string, params []*ast.Param, callArgs []ast.Expr, defaultScope *Scope) ([]string, []Type) {
	var fixed []*ast.Param
	var variadic *ast.Param
	if len(params) > 0 && params[len(params)-1].Variadic {
		variadic = params[len(params)-1]
		fixed = params[:len(params)-1]
	} else {
		fixed = params
	}

	if len(callArgs) < len(fixed) {
		for i := len(callArgs); i < len(fixed); i++ {
			if fixed[i].Default == nil {
				panic(fmt.Sprintf("nox: call to '%s': missing required argument '%s'", fname, fixed[i].Name))
			}
		}
	}
	if variadic == nil && len(callArgs) > len(fixed) {
		panic(fmt.Sprintf("nox: call to '%s': too many arguments (expected %d, got %d)", fname, len(fixed), len(callArgs)))
	}

	var codes []string
	var types []Type
	for i, p := range fixed {
		if i < len(callArgs) {
			code, t := fb.genExpr(c, callArgs[i])
			codes = append(codes, code)
			types = append(types, t)
		} else {
			dc, dpre := newCtx(defaultScope)
			code, t := fb.genExpr(dc, p.Default)
			for _, ln := range *dpre {
				c.emit(ln)
			}
			codes = append(codes, code)
			types = append(types, t)
		}
	}
	if variadic != nil {
		rest := callArgs[min(len(fixed), len(callArgs)):]
		var elemType *Type
		if variadic.Type != nil {
			t := fb.cg.resolveTypeExpr(variadic.Type)
			elemType = &t
		}
		tmp := fb.cg.freshTmp("variadic")
		c.emit(compilef("nox_array %s = nox_array_new();", tmp))
		for _, a := range rest {
			code, t := fb.genExpr(c, a)
			if elemType == nil {
				et := t
				elemType = &et
			} else if !elemType.Equals(t) {
				panic(fmt.Sprintf("nox: call to '%s': variadic argument type mismatch (%s vs %s)", fname, elemType.String(), t.String()))
			}
			etmp := fb.cg.freshTmp("velem")
			c.emit(compilef("%s %s = %s;", fb.cg.ctype(t), etmp, code))
			c.emit(compilef("nox_array_push_raw(&%s, &%s, sizeof(%s));", tmp, etmp, fb.cg.ctype(t)))
		}
		if elemType == nil {
			// No variadic args passed and no declared type: default to int
			// (an empty variadic array is otherwise untypeable).
			et := TInt()
			elemType = &et
		}
		codes = append(codes, tmp)
		types = append(types, TArray(*elemType))
	}
	return codes, types
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// callUserFunc generates a call to a top-level Nox function, instantiating
// (monomorphizing) it for the given argument types if this is the first
// time it's been called with them.
func (fb *funcBuilder) callUserFunc(c *ctx, name string, callArgs []ast.Expr) (string, Type) {
	decl, ok := fb.cg.funcsByName[name]
	if !ok {
		panic(fmt.Sprintf("nox: %s: call to undefined function '%s'", fb.fname, name))
	}
	argCodes, argTypes := fb.resolveCallArgs(c, name, decl.Params, callArgs, fb.cg.globalScope)
	fi := fb.cg.getOrInstantiateFunc(name, decl, argTypes)
	call := fmt.Sprintf("%s(%s)", fi.MangledName, strings.Join(argCodes, ", "))
	if fi.IsAsync {
		return call, TTask(fi.RetType)
	}
	return call, fi.RetType
}

// getOrInstantiateFunc returns the (possibly newly generated) monomorphized
// instance of decl for the given concrete argument types.
func (cg *Codegen) getOrInstantiateFunc(name string, decl *ast.FuncDecl, argTypes []Type) *FuncInstance {
	key := funcKey{name: name, argsKey: mangleList(argTypes)}
	if fi, ok := cg.instCache[key]; ok {
		if fi.Emitting && !fi.RetTypeKnown {
			panic(fmt.Sprintf("nox: recursive call to '%s' before its return type could be inferred; add an explicit return type annotation (func %s(...): TYPE)", name, name))
		}
		return fi
	}

	// Check parameter type compatibility for explicitly-typed parameters.
	for i, p := range decl.Params {
		if i >= len(argTypes) {
			break
		}
		if argTypes[i].ContainsUnknown() {
			panic(fmt.Sprintf("nox: call to '%s': cannot pass an empty array literal '[]' for argument '%s' without an explicit type; write it as e.g. (an array<TYPE>) or add a type to '%s's parameter", name, p.Name, name))
		}
		if p.Type != nil {
			want := cg.resolveTypeExpr(p.Type)
			if !want.Equals(argTypes[i]) {
				panic(fmt.Sprintf("nox: call to '%s': argument '%s' expects %s, got %s", name, p.Name, want.String(), argTypes[i].String()))
			}
		}
	}

	mangled := cg.freshName("nox_fn_" + sanitizeIdent(name))
	fi := &FuncInstance{MangledName: mangled, Decl: decl, ParamTypes: argTypes, IsAsync: decl.IsAsync}
	if decl.ReturnType != nil {
		fi.RetType = cg.resolveTypeExpr(decl.ReturnType)
		fi.RetTypeKnown = true
	}
	fi.Emitting = true
	cg.instCache[key] = fi
	cg.funcInstances[mangled] = fi
	cg.funcOrder = append(cg.funcOrder, mangled)

	if decl.IsAsync {
		cg.emitAsyncFunc(fi, decl, argTypes, nil)
	} else {
		cg.emitSyncFunc(fi, decl, argTypes, "this", nil)
	}
	fi.Emitting = false
	return fi
}

