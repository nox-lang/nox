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
