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

// errorCheckSnippet is appended after any statement that might have executed
// a fallible call (we conservatively add it after every simple statement).
// If an unhandled Nox error is pending: inside a try, jump to its catch
// block; otherwise propagate by returning from the function immediately
// (after running defers), like an unwinding exception.
// errorJumpCode returns the C control transfer to perform when an unhandled
// Nox error is detected: jump to the nearest enclosing try's catch handler,
// or (if none) store the zero return value and jump to the function's exit.
func (fb *funcBuilder) errorJumpCode() string {
	if len(fb.tryStack) > 0 {
		top := fb.tryStack[len(fb.tryStack)-1]
		return fmt.Sprintf("goto %s;", top.catchLabel)
	}
	return "%%RETZERO%% goto __nox_exit;"
}

func (fb *funcBuilder) errorCheckSnippet() string {
	return compilef("if (NOX_HAS_ERR) { %s }", fb.errorJumpCode())
}

// ---------------- let / assign ----------------

func (fb *funcBuilder) genLetStmt(scope *Scope, s *ast.LetStmt) string {
	c, pre := newCtx(scope)
	if s.Value == nil {
		// Uninitialized variable: type must be resolvable from annotation,
		// or inferred later from the first assignment in this scope.
		if s.Type != nil {
			t := fb.cg.resolveTypeExpr(s.Type)
			scope.define(s.Name, t)
			return compilef("%s %s = %s;", fb.cg.ctype(t), cIdent(s.Name), fb.cg.zeroValueC(t))
		}
		t, ok := fb.inferDeferredLocalType(scope, s.Name)
		if !ok {
			panic(fmt.Sprintf("nox: cannot infer type of uninitialized variable '%s' in %s; add an explicit type (let %s: TYPE)", s.Name, fb.fname, s.Name))
		}
		scope.define(s.Name, t)
		return compilef("%s %s = %s;", fb.cg.ctype(t), cIdent(s.Name), fb.cg.zeroValueC(t))
	}
	code, t := fb.genExpr(c, s.Value)
	if s.Type != nil {
		want := fb.cg.resolveTypeExpr(s.Type)
		if !want.Equals(t) {
			panic(fmt.Sprintf("nox: %s: cannot assign %s to 'let %s: %s'", fb.fname, t.String(), s.Name, want.String()))
		}
		t = want
	}
	if t.ContainsUnknown() {
		panic(fmt.Sprintf("nox: %s: cannot infer the element type of an empty array literal assigned to '%s'; add an explicit type (let %s: array<TYPE> = [])", fb.fname, s.Name, s.Name))
	}
	scope.define(s.Name, t)
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	sb.WriteString(compilef("%s %s = %s;", fb.cg.ctype(t), cIdent(s.Name), code))
	return sb.String()
}

// inferDeferredLocalType handles `let value` (no initializer, no type): scan
// forward in the *same* block for the first plain assignment to this name
// and use its type. This is a deliberate simplification of Nox's "value
// starts null until first use" rule (see project README for rationale).
func (fb *funcBuilder) inferDeferredLocalType(scope *Scope, name string) (Type, bool) {
	// We don't have direct access to "the rest of the block" here since we
	// generate statement-by-statement; callers needing this should instead
	// annotate the type explicitly. As a pragmatic fallback we default such
	// variables to a same-scope search is not implemented; require annotation.
	return Type{}, false
}

func (fb *funcBuilder) genAssignStmt(scope *Scope, s *ast.AssignStmt) string {
	c, pre := newCtx(scope)
	valCode, valType := fb.genExpr(c, s.Value)

	switch target := s.Target.(type) {
	case *ast.Ident:
		existing, ok := scope.lookup(target.Name)
		var sb strings.Builder
		for _, p := range *pre {
			sb.WriteString(p)
		}
		if !ok {
			// implicit-typed first assignment (supports `let value` deferred inference)
			if valType.ContainsUnknown() {
				panic(fmt.Sprintf("nox: %s: cannot infer the type of '%s' from an empty array literal '[]'; declare it with an explicit type first (let %s: array<TYPE>)", fb.fname, target.Name, target.Name))
			}
			scope.define(target.Name, valType)
			sb.WriteString(compilef("%s %s = %s;", fb.cg.ctype(valType), cIdent(target.Name), valCode))
			return sb.String()
		}
		op := assignOpC(s.Op, existing)
		if s.Op != token.ASSIGN && !existing.Equals(valType) {
			panic(fmt.Sprintf("nox: %s: type mismatch in compound assignment to '%s'", fb.fname, target.Name))
		}
		if s.Op == token.ASSIGN && !existing.Equals(valType) {
			panic(fmt.Sprintf("nox: %s: cannot assign %s to variable '%s' of type %s", fb.fname, valType.String(), target.Name, existing.String()))
		}
		if existing.Kind == KString && s.Op == token.PLUSEQ {
			sb.WriteString(compilef("%s = nox_string_concat(%s, %s);", cIdent(target.Name), cIdent(target.Name), valCode))
			return sb.String()
		}
		sb.WriteString(compilef("%s %s %s;", cIdent(target.Name), op, valCode))
		return sb.String()
	case *ast.IndexExpr:
		xCode, xType := fb.genExpr(c, target.X)
		idxCode, _ := fb.genExpr(c, target.Index)
		var sb strings.Builder
		for _, p := range *pre {
			sb.WriteString(p)
		}
		elemC := fb.cg.ctype(*xType.Elem)
		lvalue := fmt.Sprintf("((%s*)(%s).data)[%s]", elemC, xCode, idxCode)
		sb.WriteString(compilef("nox_array_check_index(&(%s), %s);", xCode, idxCode))
		if s.Op == token.PLUSEQ && xType.Elem.Kind == KString {
			sb.WriteString(compilef("%s = nox_string_concat(%s, %s);", lvalue, lvalue, valCode))
		} else {
			sb.WriteString(compilef("%s %s %s;", lvalue, assignOpC(s.Op, *xType.Elem), valCode))
		}
		return sb.String()
	case *ast.MemberExpr:
		xCode, xType := fb.genExpr(c, target.X)
		var sb strings.Builder
		for _, p := range *pre {
			sb.WriteString(p)
		}
		if xType.Kind != KClass {
			panic(fmt.Sprintf("nox: %s: cannot assign to member '%s' of non-class value", fb.fname, target.Name))
		}
		ci := fb.cg.classInstances[xType.ClassKey]
		ft, ok := ci.FieldTypes[target.Name]
		if ok && fieldIsPrivate(ci.Decl, target.Name) && fb.currentClassKey != xType.ClassKey {
			panic(fmt.Sprintf("nox: %s: '%s' is a private field of class '%s'", fb.fname, target.Name, ci.ClassName))
		}
		if !ok {
			if ci.StructEmitted {
				panic(fmt.Sprintf("nox: %s: class '%s' has no field '%s' (fields must be established by an assignment inside 'init')", fb.fname, ci.ClassName, target.Name))
			}
			if s.Op != token.ASSIGN {
				panic(fmt.Sprintf("nox: %s: field '%s' of class '%s' must be assigned with '=' the first time (to establish its type)", fb.fname, target.Name, ci.ClassName))
			}
			if valType.ContainsUnknown() {
				panic(fmt.Sprintf("nox: %s: cannot infer the type of field '%s' from an empty array literal '[]'; give it an explicit type (let %s: array<TYPE>) or assign a non-empty array first", fb.fname, target.Name, target.Name))
			}
			ci.FieldTypes[target.Name] = valType
			ci.FieldOrder = append(ci.FieldOrder, target.Name)
			ft = valType
		} else if !ft.Equals(valType) {
			panic(fmt.Sprintf("nox: %s: cannot assign %s to field '%s' of type %s", fb.fname, valType.String(), target.Name, ft.String()))
		}
		lvalue := fmt.Sprintf("(%s)->%s", xCode, target.Name)
		if s.Op == token.PLUSEQ && ft.Kind == KString {
			sb.WriteString(compilef("%s = nox_string_concat(%s, %s);", lvalue, lvalue, valCode))
		} else {
			sb.WriteString(compilef("%s %s %s;", lvalue, assignOpC(s.Op, ft), valCode))
		}
		return sb.String()
	}
	panic(fmt.Sprintf("nox: %s: invalid assignment target", fb.fname))
}

func assignOpC(op token.Kind, t Type) string {
	switch op {
	case token.ASSIGN:
		return "="
	case token.PLUSEQ:
		return "+="
	case token.MINUSEQ:
		return "-="
	case token.STAREQ:
		return "*="
	case token.SLASHEQ:
		return "/="
	}
	return "="
}

func cIdent(name string) string {
	return "v_" + name
}

// ---------------- if ----------------

func (fb *funcBuilder) genIfStmt(scope *Scope, s *ast.IfStmt) string {
	c, pre := newCtx(scope)
	condCode, condType := fb.genExpr(c, s.Cond)
	if condType.Kind != KBool {
		panic(fmt.Sprintf("nox: %s: if condition must be bool, got %s", fb.fname, condType.String()))
	}
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	sb.WriteString(compilef("if (%s) {", condCode))
	sb.WriteString(indent(fb.genBlock(scope, s.Then), "    "))
	if s.Else != nil {
		sb.WriteString("} else ")
		switch e := s.Else.(type) {
		case *ast.IfStmt:
			inner := fb.genIfStmt(scope, e)
			// re-indent: drop trailing newline management by concatenation
			sb.WriteString("{\n")
			sb.WriteString(indent(inner, "    "))
			sb.WriteString("}\n")
		case *ast.BlockStmt:
			sb.WriteString("{\n")
			sb.WriteString(indent(fb.genBlock(scope, e), "    "))
			sb.WriteString("}\n")
		}
	} else {
		sb.WriteString("}\n")
	}
	return sb.String()
}

// ---------------- if, as an expression ----------------
//
// `if` used as a plain statement (genIfStmt, above) is unchanged: no `else`
// is required, and its branches are ordinary statement lists. `if` used as
// an *expression* (e.g. `let x = if (c) { a } else { b }`) is a distinct,
// stricter form: every branch must be present (an `else` is mandatory) and
// each branch's final statement must be a bare value expression, which
// becomes that branch's contribution to the overall result — there is no
// `break`/`next`/`yield` involved, deliberately, so that `break`/`next`
// written inside an if used as a plain statement (overwhelmingly the more
// common case, e.g. `if (x) { break }` inside a loop) keep meaning exactly
// what they already mean and keep targeting the enclosing loop, not this
// `if`.
func (fb *funcBuilder) genIfExpr(c *ctx, s *ast.IfStmt) (string, Type) {
	resultVar := fb.cg.freshTmp("ifresult")
	var resultType *Type
	body := fb.genIfChainExpr(c.scope, s, resultVar, &resultType)
	if resultType == nil {
		panic(fmt.Sprintf("nox: %s: if-expression: could not determine a result type", fb.fname))
	}
	c.emit(compilef("%s %s;", fb.cg.ctype(*resultType), resultVar))
	c.emit(body)
	return resultVar, *resultType
}

func (fb *funcBuilder) genIfChainExpr(scope *Scope, s *ast.IfStmt, resultVar string, resultType **Type) string {
	c, pre := newCtx(scope)
	condCode, condType := fb.genExpr(c, s.Cond)
	if condType.Kind != KBool {
		panic(fmt.Sprintf("nox: %s: if condition must be bool, got %s", fb.fname, condType.String()))
	}
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	sb.WriteString(compilef("if (%s) {", condCode))
	sb.WriteString(indent(fb.genBranchExpr(scope, s.Then, resultVar, resultType), "    "))
	sb.WriteString("} else ")
	switch e := s.Else.(type) {
	case *ast.IfStmt:
		sb.WriteString("{\n")
		sb.WriteString(indent(fb.genIfChainExpr(scope, e, resultVar, resultType), "    "))
		sb.WriteString("}\n")
	case *ast.BlockStmt:
		sb.WriteString("{\n")
		sb.WriteString(indent(fb.genBranchExpr(scope, e, resultVar, resultType), "    "))
		sb.WriteString("}\n")
	default:
		panic(fmt.Sprintf("nox: %s: an if-expression must have an 'else' covering every case", fb.fname))
	}
	return sb.String()
}

// genBranchExpr compiles one branch of an if-expression: every statement
// but the last runs normally, and the last statement must be a bare value
// expression, assigned into the shared resultVar.
func (fb *funcBuilder) genBranchExpr(parentScope *Scope, block *ast.BlockStmt, resultVar string, resultType **Type) string {
	scope := newScope(parentScope)
	if len(block.Stmts) == 0 {
		panic(fmt.Sprintf("nox: %s: an if-expression branch must end with a value expression", fb.fname))
	}
	var sb strings.Builder
	for _, st := range block.Stmts[:len(block.Stmts)-1] {
		sb.WriteString(fb.genStmt(scope, st))
	}
	es, ok := block.Stmts[len(block.Stmts)-1].(*ast.ExprStmt)
	if !ok {
		panic(fmt.Sprintf("nox: %s: an if-expression branch must end with a value expression", fb.fname))
	}
	c, pre := newCtx(scope)
	code, t := fb.genExpr(c, es.X)
	for _, p := range *pre {
		sb.WriteString(p)
	}
	if *resultType == nil {
		tc := t
		*resultType = &tc
	} else if !(*resultType).Equals(t) {
		panic(fmt.Sprintf("nox: %s: if-expression branches have inconsistent types (%s vs %s)", fb.fname, (*resultType).String(), t.String()))
	}
	sb.WriteString(compilef("%s = %s;", resultVar, code))
	return sb.String()
}

// ---------------- loops ----------------

// scanLoopBody determines whether the loop body (not crossing into a nested
// loop or function literal) contains a `next value` (collect mode) and/or a
// `break value` (break-with-value mode). A `break value` (or `next value`)
// found inside a nested `switch` belongs to that switch (or, for `next`,
// still targets the outer loop — but doesn't change *this* loop's own
// collect-mode determination the way a directly-nested one would... in
// fact it does, since `next` always targets the nearest real loop even
// through a switch — see genNextStmt), so both are scanned through nested
// switches the same way `next`'s own runtime targeting works.
func scanLoopBody(b *ast.BlockStmt) (hasNext, hasBreakValue bool) {
	var walkStmts func(stmts []ast.Stmt, inSwitch bool)
	var walkStmt func(st ast.Stmt, inSwitch bool)
	walkStmt = func(st ast.Stmt, inSwitch bool) {
		switch s := st.(type) {
		case *ast.NextStmt:
			if s.Value != nil {
				hasNext = true
			}
		case *ast.BreakStmt:
			if s.Value != nil && !inSwitch {
				hasBreakValue = true
			}
		case *ast.IfStmt:
			walkStmts(s.Then.Stmts, inSwitch)
			if s.Else != nil {
				walkStmt(s.Else, inSwitch)
			}
		case *ast.BlockStmt:
			walkStmts(s.Stmts, inSwitch)
		case *ast.SwitchStmt:
			for _, c := range s.Cases {
				walkStmts(c.Body.Stmts, true)
			}
			if s.Default != nil {
				walkStmts(s.Default.Stmts, true)
			}
		case *ast.TryStmt:
			walkStmts(s.Body.Stmts, inSwitch)
			walkStmts(s.CatchBody.Stmts, inSwitch)
		// Nested loops and function literals establish a new boundary: a
		// `next`/`break value` inside them belongs to *that* construct.
		case *ast.ForCondStmt, *ast.ForInStmt, *ast.WhileStmt:
			return
		}
	}
	walkStmts = func(stmts []ast.Stmt, inSwitch bool) {
		for _, st := range stmts {
			walkStmt(st, inSwitch)
		}
	}
	walkStmts(b.Stmts, false)
	return
}

// scanSwitchBody determines whether this switch's own cases (not crossing
// into a nested loop, function literal, or nested switch) contain a
// `break value`.
func scanSwitchBody(cases []*ast.SwitchCase, def *ast.BlockStmt) (hasBreakValue bool) {
	var walkStmts func([]ast.Stmt)
	var walkStmt func(ast.Stmt)
	walkStmt = func(st ast.Stmt) {
		switch s := st.(type) {
		case *ast.BreakStmt:
			if s.Value != nil {
				hasBreakValue = true
			}
		case *ast.IfStmt:
			walkStmts(s.Then.Stmts)
			if s.Else != nil {
				walkStmt(s.Else)
			}
		case *ast.BlockStmt:
			walkStmts(s.Stmts)
		case *ast.TryStmt:
			walkStmts(s.Body.Stmts)
			walkStmts(s.CatchBody.Stmts)
		case *ast.SwitchStmt, *ast.ForCondStmt, *ast.ForInStmt, *ast.WhileStmt:
			return
		}
	}
	walkStmts = func(stmts []ast.Stmt) {
		for _, st := range stmts {
			walkStmt(st)
		}
	}
	for _, c := range cases {
		walkStmts(c.Body.Stmts)
	}
	if def != nil {
		walkStmts(def.Stmts)
	}
	return
}

func (fb *funcBuilder) beginLoop(b *ast.BlockStmt) *loopCtx {
	hasNext, hasBreakValue := scanLoopBody(b)
	if hasNext && hasBreakValue {
		panic(fmt.Sprintf("nox: %s: a loop cannot mix a value-collecting 'next' with a value-carrying 'break' in the same loop", fb.fname))
	}
	lc := &loopCtx{mode: "plain", continueLabel: fb.cg.freshTmp("continue")}
	if hasNext {
		lc.mode = "collect"
		lc.collectVar = fb.cg.freshTmp("collect")
	} else if hasBreakValue {
		lc.mode = "breakvalue"
		lc.resultVar = fb.cg.freshTmp("result")
		lc.brokeVar = fb.cg.freshTmp("broke")
	}
	fb.loopStack = append(fb.loopStack, lc)
	return lc
}

func (fb *funcBuilder) endLoop() *loopCtx {
	lc := fb.loopStack[len(fb.loopStack)-1]
	fb.loopStack = fb.loopStack[:len(fb.loopStack)-1]
	return lc
}

// assembleLoop wraps a generated C loop body with whatever preamble/epilogue
// the loop's mode requires, and reports the resulting (code, Type, isExpr).
func (fb *funcBuilder) assembleLoop(lc *loopCtx, header, bodyC string, isExprCtx bool) (string, Type, string) {
	var sb strings.Builder
	resultType := TVoid()
	valueVar := ""
	switch lc.mode {
	case "collect":
		elem := TInt()
		if lc.elemType != nil {
			elem = *lc.elemType
		}
		sb.WriteString(compilef("nox_array %s = nox_array_new();", lc.collectVar))
		sb.WriteString(header + " {\n")
		sb.WriteString(indent(bodyC, "    "))
		sb.WriteString("}\n")
		resultType = TArray(elem)
		valueVar = lc.collectVar
	case "breakvalue":
		rt := TInt()
		if lc.resultType != nil {
			rt = *lc.resultType
		}
		sb.WriteString(compilef("%s %s = %s;", fb.cg.ctype(rt), lc.resultVar, fb.cg.zeroValueC(rt)))
		sb.WriteString(compilef("bool %s = false;", lc.brokeVar))
		sb.WriteString(header + " {\n")
		sb.WriteString(indent(bodyC, "    "))
		sb.WriteString("}\n")
		resultType = rt
		valueVar = lc.resultVar
	default:
		sb.WriteString(header + " {\n")
		sb.WriteString(indent(bodyC, "    "))
		sb.WriteString("}\n")
		if isExprCtx {
			panic(fmt.Sprintf("nox: %s: this loop/switch produces no value (no collecting 'next' or value-carrying 'break') but is used as an expression", fb.fname))
		}
	}
	return sb.String(), resultType, valueVar
}

func (fb *funcBuilder) genForCond(scope *Scope, s *ast.ForCondStmt, isExprCtx bool) (string, Type, string) {
	c, pre := newCtx(scope)
	condCode := "1"
	if s.Cond != nil {
		var ct Type
		condCode, ct = fb.genExpr(c, s.Cond)
		if ct.Kind != KBool {
			panic(fmt.Sprintf("nox: %s: for-condition must be bool", fb.fname))
		}
	}
	var preSB strings.Builder
	for _, p := range *pre {
		preSB.WriteString(p)
	}
	lc := fb.beginLoop(s.Body)
	bodyC := fb.genBlock(scope, s.Body)
	bodyC += compilef("%s: ;", lc.continueLabel)
	fb.endLoop()
	header := fmt.Sprintf("for (; %s; )", condCode)
	// re-evaluate condition each iteration if it references mutable state:
	// since Nox conditions are arbitrary expressions (not just idents), we
	// must recompute them every loop iteration, not hoist once. Use a while
	// wrapper that re-runs the condition's `pre` computations each pass.
	if len(*pre) != 0 {
		// condition has side-effecting pre-statements: emit as for(;;){ pre; if(!cond) break; body }
		var innerSB strings.Builder
		for _, p := range *pre {
			innerSB.WriteString(p)
		}
		innerSB.WriteString(compilef("if (!(%s)) break;", condCode))
		innerSB.WriteString(bodyC)
		loopCode, rt, vv := fb.assembleLoop(lc, "for (;;)", innerSB.String(), isExprCtx)
		return preSB.String() + loopCode, rt, vv
	}
	loopCode, rt, vv := fb.assembleLoop(lc, header, bodyC, isExprCtx)
	return preSB.String() + loopCode, rt, vv
}

func (fb *funcBuilder) genWhile(scope *Scope, s *ast.WhileStmt, isExprCtx bool) (string, Type, string) {
	c, pre := newCtx(scope)
	condCode, ct := fb.genExpr(c, s.Cond)
	if ct.Kind != KBool {
		panic(fmt.Sprintf("nox: %s: while-condition must be bool", fb.fname))
	}
	lc := fb.beginLoop(s.Body)
	bodyC := fb.genBlock(scope, s.Body)
	bodyC += compilef("%s: ;", lc.continueLabel)
	fb.endLoop()
	if len(*pre) == 0 {
		header := fmt.Sprintf("while (%s)", condCode)
		return fb.assembleLoop(lc, header, bodyC, isExprCtx)
	}
	var innerSB strings.Builder
	for _, p := range *pre {
		innerSB.WriteString(p)
	}
	innerSB.WriteString(compilef("if (!(%s)) break;", condCode))
	innerSB.WriteString(bodyC)
	return fb.assembleLoop(lc, "for (;;)", innerSB.String(), isExprCtx)
}

func (fb *funcBuilder) genForIn(scope *Scope, s *ast.ForInStmt, isExprCtx bool) (string, Type, string) {
	c, pre := newCtx(scope)
	arrCode, arrType := fb.genExpr(c, s.Array)
	if arrType.Kind != KArray {
		panic(fmt.Sprintf("nox: %s: 'for (... in ...)' requires an array, got %s", fb.fname, arrType.String()))
	}
	elemType := *arrType.Elem
	var preSB strings.Builder
	for _, p := range *pre {
		preSB.WriteString(p)
	}
	arrTmp := fb.cg.freshTmp("arr")
	preSB.WriteString(compilef("nox_array %s = %s;", arrTmp, arrCode))

	inner := newScope(scope)
	idxVar := fb.cg.freshTmp("i")
	if s.IndexName != "" {
		inner.define(s.IndexName, TInt())
	}
	inner.define(s.ValueName, elemType)

	lc := fb.beginLoop(s.Body)
	var bodySB strings.Builder
	bodySB.WriteString(compilef("%s %s = ((%s*)%s.data)[%s];", fb.cg.ctype(elemType), cIdent(s.ValueName), fb.cg.ctype(elemType), arrTmp, idxVar))
	if s.IndexName != "" {
		bodySB.WriteString(compilef("%s %s = %s;", fb.cg.ctype(TInt()), cIdent(s.IndexName), idxVar))
	}
	bodySB.WriteString(fb.genBlock(inner, s.Body))
	bodySB.WriteString(compilef("%s: ;", lc.continueLabel))
	fb.endLoop()

	header := fmt.Sprintf("for (int64_t %s = 0; %s < %s.len; %s++)", idxVar, idxVar, arrTmp, idxVar)
	loopCode, rt, vv := fb.assembleLoop(lc, header, bodySB.String(), isExprCtx)
	return preSB.String() + loopCode, rt, vv
}

// ---------------- switch ----------------

func eqExprC(aC string, aT Type, bC string, bT Type) string {
	if !aT.Equals(bT) {
		panic(fmt.Sprintf("nox: switch: case value type %s does not match subject type %s", bT.String(), aT.String()))
	}
	if aT.Kind == KString {
		return fmt.Sprintf("nox_string_eq(%s, %s)", aC, bC)
	}
	return fmt.Sprintf("(%s == %s)", aC, bC)
}

func (fb *funcBuilder) genSwitch(scope *Scope, s *ast.SwitchStmt, isExprCtx bool) (string, Type, string) {
	c, pre := newCtx(scope)
	subjCode, subjType := fb.genExpr(c, s.Subject)
	var preSB strings.Builder
	for _, p := range *pre {
		preSB.WriteString(p)
	}
	subjTmp := fb.cg.freshTmp("switchval")
	preSB.WriteString(compilef("%s %s = %s;", fb.cg.ctype(subjType), subjTmp, subjCode))

	hasBreakValue := scanSwitchBody(s.Cases, s.Default)
	lc := &loopCtx{mode: "plain", isSwitch: true}
	if hasBreakValue {
		lc.mode = "breakvalue"
		lc.resultVar = fb.cg.freshTmp("result")
		lc.brokeVar = fb.cg.freshTmp("broke")
	}
	fb.loopStack = append(fb.loopStack, lc)

	var bodySB strings.Builder
	first := true
	for _, cs := range s.Cases {
		var conds []string
		for _, v := range cs.Values {
			c2, pre2 := newCtx(scope)
			code, t := fb.genExpr(c2, v)
			if len(*pre2) > 0 {
				panic(fmt.Sprintf("nox: %s: switch 'case' values must be simple expressions", fb.fname))
			}
			conds = append(conds, eqExprC(subjTmp, subjType, code, t))
		}
		condC := strings.Join(conds, " || ")
		if first {
			bodySB.WriteString(compilef("if (%s) {", condC))
			first = false
		} else {
			bodySB.WriteString(compilef("} else if (%s) {", condC))
		}
		bodySB.WriteString(indent(fb.genBlock(scope, cs.Body), "    "))
	}
	if s.Default != nil {
		if first {
			bodySB.WriteString("{\n")
			first = false
		} else {
			bodySB.WriteString("} else {\n")
		}
		bodySB.WriteString(indent(fb.genBlock(scope, s.Default), "    "))
	}
	if !first {
		bodySB.WriteString("}\n")
	}
	fb.loopStack = fb.loopStack[:len(fb.loopStack)-1]

	fullBody := bodySB.String() + "break;\n"
	loopCode, rt, vv := fb.assembleLoop(lc, "for (;;)", fullBody, isExprCtx)
	return preSB.String() + loopCode, rt, vv
}

func (fb *funcBuilder) genBreakStmt(scope *Scope, s *ast.BreakStmt) string {
	if len(fb.loopStack) == 0 {
		panic(fmt.Sprintf("nox: %s: 'break' used outside of a loop", fb.fname))
	}
	lc := fb.loopStack[len(fb.loopStack)-1]
	if s.Value == nil {
		return "break;\n"
	}
	c, pre := newCtx(scope)
	code, t := fb.genExpr(c, s.Value)
	if lc.mode != "breakvalue" {
		panic(fmt.Sprintf("nox: %s: 'break <value>' is not valid here", fb.fname))
	}
	if lc.resultType == nil {
		rt := t
		lc.resultType = &rt
	} else if !lc.resultType.Equals(t) {
		panic(fmt.Sprintf("nox: %s: inconsistent 'break' value types in the same loop (%s vs %s)", fb.fname, lc.resultType.String(), t.String()))
	}
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	sb.WriteString(compilef("%s = %s; %s = true; break;", lc.resultVar, code, lc.brokeVar))
	return sb.String()
}

// genNextStmt implements a loop's `next` (like C's `continue`) and
// `next value` (continue, but first collect `value` into the loop's result
// array — this is what used to be spelled `return value` inside a loop).
// It always jumps via each real loop's own continueLabel (see beginLoop)
// rather than emitting a bare C `continue`, so it behaves correctly even
// when reached through an intervening `switch` (which is internally
// implemented as its own tiny loop-like construct that a raw `continue`
// would incorrectly target instead of the real outer loop).
func (fb *funcBuilder) genNextStmt(scope *Scope, s *ast.NextStmt) string {
	var lc *loopCtx
	for i := len(fb.loopStack) - 1; i >= 0; i-- {
		if !fb.loopStack[i].isSwitch {
			lc = fb.loopStack[i]
			break
		}
	}
	if lc == nil {
		panic(fmt.Sprintf("nox: %s: 'next' used outside of a loop", fb.fname))
	}
	if s.Value == nil {
		return compilef("goto %s;", lc.continueLabel)
	}
	if lc.mode != "collect" {
		panic(fmt.Sprintf("nox: %s: 'next <value>' is not valid here (this loop was not detected as collecting; move any other 'next <value>'/'break <value>' consistently within it)", fb.fname))
	}
	c, pre := newCtx(scope)
	code, t := fb.genExpr(c, s.Value)
	if lc.elemType == nil {
		et := t
		lc.elemType = &et
	} else if !lc.elemType.Equals(t) {
		panic(fmt.Sprintf("nox: %s: inconsistent collected 'next' value types in the same loop (%s vs %s)", fb.fname, lc.elemType.String(), t.String()))
	}
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	tmp := fb.cg.freshTmp("nv")
	sb.WriteString(compilef("%s %s = %s;", fb.cg.ctype(t), tmp, code))
	sb.WriteString(compilef("nox_array_push_raw(&%s, &%s, sizeof(%s));", lc.collectVar, tmp, fb.cg.ctype(t)))
	sb.WriteString(compilef("goto %s;", lc.continueLabel))
	return sb.String()
}

// genYieldStmt implements `yield value` inside an
// each/eachIndex/map/filter/find callback or a sort comparator: it supplies
// this invocation's result without exiting the enclosing Nox function
// (unlike `return`, which — uniformly, everywhere — always does).
func (fb *funcBuilder) genYieldStmt(scope *Scope, s *ast.YieldStmt) string {
	var lc *loopCtx
	for i := len(fb.loopStack) - 1; i >= 0; i-- {
		if !fb.loopStack[i].isSwitch {
			lc = fb.loopStack[i]
			break
		}
	}
	if lc == nil || lc.mode != "hofvalue" {
		panic(fmt.Sprintf("nox: %s: 'yield' used outside of an each/map/filter/find callback or a sort comparator", fb.fname))
	}
	c, pre := newCtx(scope)
	code, t := fb.genExpr(c, s.Value)
	if lc.resultType == nil {
		et := t
		lc.resultType = &et
	} else if !lc.resultType.Equals(t) {
		panic(fmt.Sprintf("nox: %s: inconsistent 'yield' value types in the same callback (%s vs %s)", fb.fname, lc.resultType.String(), t.String()))
	}
	var sb strings.Builder
	for _, p := range *pre {
		sb.WriteString(p)
	}
	sb.WriteString(compilef("%s = %s;", lc.resultVar, code))
	sb.WriteString(compilef("goto %s;", lc.hofLabel))
	return sb.String()
}

// emitReturn generates a real function-level return: store the value (if
// any) into the function's single return slot and jump to the function's
// single exit point, where defers run exactly once.
func (fb *funcBuilder) emitReturn(scope *Scope, valueExpr ast.Expr) string {
	var sb strings.Builder
	if valueExpr != nil {
		c, pre := newCtx(scope)
		code, t := fb.genExpr(c, valueExpr)
		for _, p := range *pre {
			sb.WriteString(p)
		}
		if !fb.retTypeKnown {
			fb.retType = t
			fb.retTypeKnown = true
			if fb.selfInstance != nil {
				fb.selfInstance.RetType = t
				fb.selfInstance.RetTypeKnown = true
			}
		} else if !fb.retType.Equals(t) {
			panic(fmt.Sprintf("nox: %s: inconsistent return types (%s vs %s)", fb.fname, fb.retType.String(), t.String()))
		}
		sb.WriteString(compilef("__ret = %s;", code))
	} else {
		if !fb.retTypeKnown {
			fb.retType = TVoid()
			fb.retTypeKnown = true
			if fb.selfInstance != nil {
				fb.selfInstance.RetType = TVoid()
				fb.selfInstance.RetTypeKnown = true
			}
		} else if fb.retType.Kind != KVoid {
			panic(fmt.Sprintf("nox: %s: bare 'return' in a function that returns %s", fb.fname, fb.retType.String()))
		}
	}
	sb.WriteString("goto __nox_exit;\n")
	return sb.String()
}

func (fb *funcBuilder) deferPrologue() string {
	var sb strings.Builder
	for i := len(fb.defers) - 1; i >= 0; i-- {
		d := fb.defers[i]
		sb.WriteString(compilef("if (%s) {", d.flagVar))
		sb.WriteString(indent(d.bodyC, "    "))
		sb.WriteString("}\n")
	}
	return sb.String()
}

// buildFunctionBody generates the full `{ ... }` contents for a function:
// defer-flag declarations, the body itself, and a single exit point that
// runs armed defers and returns. It resolves the deferred %%RETDECL%% /
// %%RETZERO%% / %%RETFINAL%% placeholders once the return type is known
// (which may only become known partway through generating the body itself,
// e.g. when the type is inferred from the function's own return statements).
func (fb *funcBuilder) buildFunctionBody(scope *Scope, body *ast.BlockStmt, retFinalOverride string) string {
	declFlags := fb.prepareDefers(body)
	bodyC := fb.genBlock(scope, body)

	var sb strings.Builder
	sb.WriteString("%%RETDECL%%\n")
	sb.WriteString(declFlags)
	sb.WriteString(bodyC)
	sb.WriteString("__nox_exit: ;\n")
	sb.WriteString(fb.deferPrologue())
	sb.WriteString("%%RETFINAL%%\n")

	if !fb.retTypeKnown {
		fb.retType = TVoid()
		fb.retTypeKnown = true
	}
	retDecl := ""
	retZero := ""
	retFinal := "return;"
	if fb.retType.Kind != KVoid {
		retDecl = fmt.Sprintf("%s __ret = %s;", fb.cg.ctype(fb.retType), fb.cg.zeroValueC(fb.retType))
		retZero = fmt.Sprintf("__ret = %s;", fb.cg.zeroValueC(fb.retType))
		retFinal = "return __ret;"
	}
	if retFinalOverride != "" {
		retFinal = retFinalOverride
	}
	out := strings.ReplaceAll(sb.String(), "%%RETDECL%%", retDecl)
	out = strings.ReplaceAll(out, "%%RETZERO%%", retZero)
	out = strings.ReplaceAll(out, "%%RETFINAL%%", retFinal)
	return out
}

func (fb *funcBuilder) genTryStmt(scope *Scope, s *ast.TryStmt) string {
	catchLabel := fb.cg.freshTmp("catch")
	endLabel := fb.cg.freshTmp("tryend")
	fb.tryStack = append(fb.tryStack, &tryCtx{catchLabel: catchLabel})
	bodyC := fb.genBlock(scope, s.Body)
	fb.tryStack = fb.tryStack[:len(fb.tryStack)-1]

	catchScope := newScope(scope)
	catchScope.define(s.CatchVar, TString())
	var catchBodySB strings.Builder
	catchBodySB.WriteString(compilef("nox_string %s = nox_get_error_message();", cIdent(s.CatchVar)))
	catchBodySB.WriteString("nox_clear_error();\n")
	catchBodySB.WriteString(fb.genBlock(catchScope, s.CatchBody))

	var sb strings.Builder
	sb.WriteString("{\n")
	sb.WriteString(indent(bodyC, "    "))
	sb.WriteString(compilef("    goto %s;", endLabel))
	sb.WriteString("}\n")
	sb.WriteString(compilef("%s: ;", catchLabel))
	sb.WriteString("{\n")
	sb.WriteString(indent(catchBodySB.String(), "    "))
	sb.WriteString("}\n")
	sb.WriteString(compilef("%s: ;", endLabel))
	return sb.String()
}

// ---------------- defer ----------------

// scanDefers counts `defer` statements belonging to this function's own
// scope (i.e. not inside a nested FuncLit, which is never reached by this
// walk since FuncLit only ever appears inside expressions).
func scanDefers(b *ast.BlockStmt) int {
	count := 0
	var walkStmts func([]ast.Stmt)
	var walkStmt func(ast.Stmt)
	walkStmt = func(st ast.Stmt) {
		switch s := st.(type) {
		case *ast.DeferStmt:
			count++
		case *ast.IfStmt:
			walkStmts(s.Then.Stmts)
			if s.Else != nil {
				walkStmt(s.Else)
			}
		case *ast.BlockStmt:
			walkStmts(s.Stmts)
		case *ast.ForCondStmt:
			walkStmts(s.Body.Stmts)
		case *ast.ForInStmt:
			walkStmts(s.Body.Stmts)
		case *ast.WhileStmt:
			walkStmts(s.Body.Stmts)
		case *ast.SwitchStmt:
			for _, c := range s.Cases {
				walkStmts(c.Body.Stmts)
			}
			if s.Default != nil {
				walkStmts(s.Default.Stmts)
			}
		case *ast.TryStmt:
			walkStmts(s.Body.Stmts)
			walkStmts(s.CatchBody.Stmts)
		}
	}
	walkStmts = func(stmts []ast.Stmt) {
		for _, st := range stmts {
			walkStmt(st)
		}
	}
	walkStmts(b.Stmts)
	return count
}

// prepareDefers must be called once, before generating a function's top
// level body, so that all defer-armed flags exist (declared false) before
// any code that might reference them in a return's defer-prologue.
func (fb *funcBuilder) prepareDefers(body *ast.BlockStmt) string {
	n := scanDefers(body)
	var sb strings.Builder
	for i := 0; i < n; i++ {
		name := fb.cg.freshTmp("defer_armed")
		fb.deferFlagQueue = append(fb.deferFlagQueue, name)
		sb.WriteString(compilef("bool %s = false;", name))
	}
	return sb.String()
}

func (fb *funcBuilder) genDeferStmt(scope *Scope, s *ast.DeferStmt) string {
	if fb.deferFlagIdx >= len(fb.deferFlagQueue) {
		panic("nox: internal error: defer flag queue exhausted")
	}
	flagVar := fb.deferFlagQueue[fb.deferFlagIdx]
	fb.deferFlagIdx++
	bodyC := fb.genBlock(scope, s.Body)
	fb.defers = append(fb.defers, &deferEntry{flagVar: flagVar, bodyC: bodyC})
	return compilef("%s = true;", flagVar)
}
