package codegen

import (
	"fmt"
	"sort"
	"strings"

	"nox/internal/ast"
)

// freeVarNames returns the sorted set of identifiers referenced inside body
// that are not one of params and not locally bound somewhere in body (by
// `let`, a for-loop variable, or a try/catch variable). It is a
// syntax-level approximation (it does not model textual ordering/shadowing
// precisely) that only ever under-excludes bound names, never over-excludes
// free ones, so at worst it captures a variable unnecessarily rather than
// missing a real capture.
func freeVarNames(body *ast.BlockStmt, params []*ast.Param) []string {
	bound := map[string]bool{}
	for _, p := range params {
		bound[p.Name] = true
	}
	used := map[string]bool{}

	var walkExpr func(ast.Expr)
	var walkStmt func(ast.Stmt)
	walkStmts := func(stmts []ast.Stmt) {
		for _, s := range stmts {
			walkStmt(s)
		}
	}
	walkStmt = func(st ast.Stmt) {
