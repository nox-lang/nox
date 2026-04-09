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
		switch s := st.(type) {
		case *ast.LetStmt:
			if s.Value != nil {
				walkExpr(s.Value)
			}
			bound[s.Name] = true
		case *ast.ExprStmt:
			walkExpr(s.X)
		case *ast.AssignStmt:
			walkExpr(s.Target)
			walkExpr(s.Value)
		case *ast.IfStmt:
			walkExpr(s.Cond)
			walkStmts(s.Then.Stmts)
			if s.Else != nil {
				walkStmt(s.Else)
			}
		case *ast.BlockStmt:
			walkStmts(s.Stmts)
		case *ast.ForCondStmt:
			if s.Cond != nil {
				walkExpr(s.Cond)
			}
			walkStmts(s.Body.Stmts)
		case *ast.ForInStmt:
			walkExpr(s.Array)
			if s.IndexName != "" {
				bound[s.IndexName] = true
			}
			bound[s.ValueName] = true
			walkStmts(s.Body.Stmts)
		case *ast.WhileStmt:
			walkExpr(s.Cond)
			walkStmts(s.Body.Stmts)
		case *ast.BreakStmt:
			if s.Value != nil {
				walkExpr(s.Value)
			}
		case *ast.ReturnStmt:
			if s.Value != nil {
				walkExpr(s.Value)
			}
		case *ast.SwitchStmt:
			walkExpr(s.Subject)
			for _, cs := range s.Cases {
				for _, v := range cs.Values {
					walkExpr(v)
				}
				walkStmts(cs.Body.Stmts)
			}
			if s.Default != nil {
				walkStmts(s.Default.Stmts)
			}
		case *ast.DeferStmt:
			walkStmts(s.Body.Stmts)
		case *ast.TryStmt:
			walkStmts(s.Body.Stmts)
			bound[s.CatchVar] = true
			walkStmts(s.CatchBody.Stmts)
		}
	}
	walkExpr = func(e ast.Expr) {
		switch x := e.(type) {
		case *ast.Ident:
