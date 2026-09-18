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

