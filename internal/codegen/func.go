package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
	"nox/internal/token"
)

// funcBuilder holds the mutable state needed while generating the C code for
// one function/method body (or the top-level main/global-init pseudo body).
type funcBuilder struct {
	cg    *Codegen
	fname string

	currentClassKey string        // set while generating a class method body, for private-access checks
	selfInstance    *FuncInstance // back-reference so a self-recursive call can see the return type as soon as it's known, not just after the whole body finishes

	retTypeKnown bool
	retType      Type
	isAsync      bool

	defers    []*deferEntry
	loopStack []*loopCtx
	tryStack  []*tryCtx

	deferFlagQueue []string
	deferFlagIdx   int
}

type tryCtx struct {
	catchLabel string
}

type deferEntry struct {
	flagVar string
	bodyC   string
}

type loopCtx struct {
	mode          string // "collect" | "breakvalue" | "hofvalue" | "plain"
	isSwitch      bool   // true if this context represents a `switch`, not a loop
	collectVar    string
	elemType      *Type
	resultVar     string
	brokeVar      string
	resultType    *Type
	hofLabel      string // for "hofvalue": label to jump to after storing the result
	continueLabel string // for real loops: label `next`/`next value` jumps to
}

// ctx threads a per-statement hoisting buffer through expression codegen.
type ctx struct {
	scope *Scope
	pre   *[]string
}

func newCtx(scope *Scope) (*ctx, *[]string) {
	pre := []string{}
	return &ctx{scope: scope, pre: &pre}, &pre
}

func (c *ctx) emit(line string) { *c.pre = append(*c.pre, line) }

func compilef(format string, args ...interface{}) string {
	s := fmt.Sprintf(format, args...)
	if !strings.HasSuffix(s, "\n") {
		s += "\n"
	}
	return s
}

// ---------------- blocks & statement dispatch ----------------

func (fb *funcBuilder) genBlock(parent *Scope, b *ast.BlockStmt) string {
	scope := newScope(parent)
	var sb strings.Builder
	for _, st := range b.Stmts {
		sb.WriteString(fb.genStmt(scope, st))
	}
	return sb.String()
}

func (fb *funcBuilder) genStmt(scope *Scope, st ast.Stmt) string {
	switch s := st.(type) {
	case *ast.LetStmt:
		return fb.genLetStmt(scope, s) + fb.errorCheckSnippet()
	case *ast.ExprStmt:
		c, pre := newCtx(scope)
		code, t := fb.genExpr(c, s.X)
		_ = t
		var sb strings.Builder
		for _, p := range *pre {
			sb.WriteString(p)
		}
		if code != "" {
			sb.WriteString(compilef("%s;", code))
		}
		return sb.String() + fb.errorCheckSnippet()
	case *ast.AssignStmt:
		return fb.genAssignStmt(scope, s) + fb.errorCheckSnippet()
	case *ast.IfStmt:
		return fb.genIfStmt(scope, s)
	case *ast.ForCondStmt:
		code, _, _ := fb.genForCond(scope, s, false)
		return code
	case *ast.ForInStmt:
		code, _, _ := fb.genForIn(scope, s, false)
		return code
	case *ast.WhileStmt:
		code, _, _ := fb.genWhile(scope, s, false)
		return code
	case *ast.BreakStmt:
		return fb.genBreakStmt(scope, s)
	case *ast.NextStmt:
		return fb.genNextStmt(scope, s)
	case *ast.YieldStmt:
		return fb.genYieldStmt(scope, s)
	case *ast.ReturnStmt:
		return fb.emitReturn(scope, s.Value)
	case *ast.SwitchStmt:
		code, _, _ := fb.genSwitch(scope, s, false)
		return code
	case *ast.DeferStmt:
		return fb.genDeferStmt(scope, s)
	case *ast.TryStmt:
		return fb.genTryStmt(scope, s)
	case *ast.BlockStmt:
		return fb.genBlock(scope, s)
	}
	panic(fmt.Sprintf("codegen: unhandled statement %T", st))
}

