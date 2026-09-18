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
			used[x.Name] = true
		case *ast.ThisExpr:
			used["this"] = true
		case *ast.BinaryExpr:
			walkExpr(x.X)
			walkExpr(x.Y)
		case *ast.UnaryExpr:
			walkExpr(x.X)
		case *ast.CallExpr:
			walkExpr(x.Callee)
			for _, a := range x.Args {
				walkExpr(a)
			}
		case *ast.IndexExpr:
			walkExpr(x.X)
			walkExpr(x.Index)
		case *ast.MemberExpr:
			walkExpr(x.X)
		case *ast.ArrayLit:
			for _, el := range x.Elems {
				walkExpr(el)
			}
		case *ast.PropagateExpr:
			walkExpr(x.X)
		case *ast.AwaitExpr:
			walkExpr(x.X)
		case *ast.ParallelExpr:
			for _, ce := range x.Calls {
				walkExpr(ce)
			}
		case *ast.FuncLit:
			for _, n := range freeVarNames(x.Body, x.Params) {
				used[n] = true
			}
		case *ast.ForCondStmt, *ast.ForInStmt, *ast.WhileStmt, *ast.SwitchStmt:
			// these implement both Stmt and Expr (used as expressions); walk
			// them via walkStmt's logic by wrapping in a throwaway block.
			walkStmt(e.(ast.Stmt))
		}
	}
	walkStmts(body.Stmts)

	var free []string
	for n := range used {
		if !bound[n] {
			free = append(free, n)
		}
	}
	sort.Strings(free)
	return free
}

func retPtrOrNil(t Type) *Type {
	if t.Kind == KVoid {
		return nil
	}
	cp := t
	return &cp
}

// genFuncLitValue compiles an anonymous function literal used as a
// first-class value (assigned to a `let`, stored, or passed to something
// other than a directly-inlined built-in higher-order method) into a
// closure: a small generated top-level C function plus a captured-variable
// environment struct, represented at runtime as a `{fn, env}` fat pointer.
func (fb *funcBuilder) genFuncLitValue(c *ctx, x *ast.FuncLit) (string, Type) {
	free := freeVarNames(x.Body, x.Params)
	var capturedNames []string
	var capturedTypes []Type
	for _, n := range free {
		if t, ok := c.scope.lookup(n); ok {
			capturedNames = append(capturedNames, n)
			capturedTypes = append(capturedTypes, t)
		}
	}

	var paramTypes []Type
	for _, p := range x.Params {
		if p.Type == nil {
			panic(fmt.Sprintf("nox: %s: an anonymous function stored in a variable needs explicit parameter types, e.g. (x: int) { ... }", fb.fname))
		}
		paramTypes = append(paramTypes, fb.cg.resolveTypeExpr(p.Type))
	}

	mangled := fb.cg.freshName("nox_closure")
	var envStructName string
	if len(capturedNames) > 0 {
		envStructName = mangled + "__env"
		var fields strings.Builder
		for i, n := range capturedNames {
			fields.WriteString(fmt.Sprintf("    %s %s;\n", fb.cg.ctype(capturedTypes[i]), cIdent(n)))
		}
		fb.cg.typeDefs = append(fb.cg.typeDefs, fmt.Sprintf("typedef struct {\n%s} %s;", fields.String(), envStructName))
	}

	innerScope := newScope(nil)
	for i, n := range capturedNames {
		innerScope.define(n, capturedTypes[i])
	}
	for i, p := range x.Params {
		innerScope.define(p.Name, paramTypes[i])
	}
	innerFB := &funcBuilder{cg: fb.cg, fname: mangled}
	if x.ReturnType != nil {
		innerFB.retType = fb.cg.resolveTypeExpr(x.ReturnType)
		innerFB.retTypeKnown = true
	}
	var unpackPrefix strings.Builder
	if len(capturedNames) > 0 {
		unpackPrefix.WriteString(fmt.Sprintf("%s* __env = (%s*)__envp;\n", envStructName, envStructName))
		for i, n := range capturedNames {
			unpackPrefix.WriteString(fmt.Sprintf("%s %s = __env->%s;\n", fb.cg.ctype(capturedTypes[i]), cIdent(n), cIdent(n)))
		}
	}
	bodyC := innerFB.buildFunctionBody(innerScope, x.Body, "")
	fullBody := unpackPrefix.String() + bodyC

	retC := "void"
	if innerFB.retType.Kind != KVoid {
		retC = fb.cg.ctype(innerFB.retType)
	}
	cparams := []string{"void* __envp"}
	for i, p := range x.Params {
		cparams = append(cparams, fmt.Sprintf("%s %s", fb.cg.ctype(paramTypes[i]), cIdent(p.Name)))
	}
	forward := fmt.Sprintf("static %s %s(%s);", retC, mangled, strings.Join(cparams, ", "))
	def := fmt.Sprintf("static %s %s(%s) {\n%s}", retC, mangled, strings.Join(cparams, ", "), indent(fullBody, "    "))
	fi := &FuncInstance{MangledName: mangled, Forward: forward, Body: def, RetType: innerFB.retType, RetTypeKnown: true, ParamTypes: paramTypes}
	fb.cg.funcInstances[mangled] = fi
	fb.cg.funcOrder = append(fb.cg.funcOrder, mangled)

	fnType := Type{Kind: KFunc, Params: paramTypes, Ret: retPtrOrNil(innerFB.retType)}
	ctypeName := fb.cg.ctype(fnType) // ensures the closure fat-pointer struct typedef is registered

	envExpr := "NULL"
	if len(capturedNames) > 0 {
		envTmp := fb.cg.freshTmp("env")
		c.emit(compilef("%s* %s = (%s*)GC_MALLOC(sizeof(%s));", envStructName, envTmp, envStructName, envStructName))
		for _, n := range capturedNames {
			c.emit(compilef("%s->%s = %s;", envTmp, cIdent(n), cIdent(n)))
		}
		envExpr = envTmp
	}
	closTmp := fb.cg.freshTmp("clo")
	c.emit(compilef("%s %s;", ctypeName, closTmp))
	c.emit(compilef("%s.fn = %s;", closTmp, mangled))
	c.emit(compilef("%s.env = %s;", closTmp, envExpr))
	return closTmp, fnType
}

// ---------------- ? / await / Parallel ----------------

