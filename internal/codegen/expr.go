package codegen

import (
	"fmt"
	"strconv"
	"strings"

	"nox/internal/ast"
	"nox/internal/token"
)

func (fb *funcBuilder) genExpr(c *ctx, e ast.Expr) (string, Type) {
	switch x := e.(type) {
	case *ast.IntLit:
		return fmt.Sprintf("%dLL", x.Value), TInt()
	case *ast.FloatLit:
		return formatFloatLiteral(x.Value), TFloat()
	case *ast.StringLit:
		return fmt.Sprintf(`nox_string_from_cstr(%s)`, cStringLiteral(x.Value)), TString()
	case *ast.BoolLit:
		if x.Value {
			return "true", TBool()
		}
		return "false", TBool()
	case *ast.NullLit:
		panic("nox: explicit 'null' cannot appear in an expression; declare an uninitialized variable with 'let name: Type' instead")
	case *ast.ThisExpr:
		t, ok := c.scope.lookup("this")
		if !ok {
			panic("nox: 'this' used outside of a method")
		}
		return cIdent("this"), t
	case *ast.Ident:
		return fb.genIdent(c, x)
	case *ast.QualIdent:
		return fb.genQualIdentValue(c, x)
	case *ast.ArrayLit:
		return fb.genArrayLit(c, x)
	case *ast.BinaryExpr:
		return fb.genBinaryExpr(c, x)
	case *ast.UnaryExpr:
		return fb.genUnaryExpr(c, x)
	case *ast.CallExpr:
		return fb.genCallExpr(c, x)
	case *ast.IndexExpr:
		return fb.genIndexExpr(c, x)
	case *ast.MemberExpr:
		return fb.genMemberRead(c, x)
	case *ast.FuncLit:
		return fb.genFuncLitValue(c, x)
	case *ast.PropagateExpr:
		return fb.genPropagateExpr(c, x)
	case *ast.AwaitExpr:
		return fb.genAwaitExpr(c, x)
	case *ast.ParallelExpr:
		return fb.genParallelExpr(c, x)
	case *ast.ForCondStmt:
		code, t, vv := fb.genForCond(c.scope, x, true)
		c.emit(code)
		return vv, t
	case *ast.ForInStmt:
		code, t, vv := fb.genForIn(c.scope, x, true)
		c.emit(code)
		return vv, t
	case *ast.WhileStmt:
		code, t, vv := fb.genWhile(c.scope, x, true)
		c.emit(code)
		return vv, t
	case *ast.SwitchStmt:
		code, t, vv := fb.genSwitch(c.scope, x, true)
		c.emit(code)
		return vv, t
	case *ast.IfStmt:
		return fb.genIfExpr(c, x)
	}
	panic(fmt.Sprintf("codegen: unhandled expression %T", e))
}

func formatFloatLiteral(v float64) string {
	s := strconv.FormatFloat(v, 'g', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	return s
}

func cStringLiteral(s string) string {
	var sb strings.Builder
	sb.WriteByte('"')
	for _, r := range s {
		switch r {
		case '"':
			sb.WriteString(`\"`)
		case '\\':
			sb.WriteString(`\\`)
		case '\n':
			sb.WriteString(`\n`)
		case '\t':
			sb.WriteString(`\t`)
		case '\r':
			sb.WriteString(`\r`)
		case 0:
			sb.WriteString(`\0`)
		default:
			if r < 32 {
				sb.WriteString(fmt.Sprintf(`\x%02x`, r))
			} else {
				sb.WriteRune(r)
			}
		}
	}
	sb.WriteByte('"')
	return sb.String()
}

// ---------------- identifiers ----------------

func (fb *funcBuilder) genIdent(c *ctx, x *ast.Ident) (string, Type) {
	if t, ok := c.scope.lookup(x.Name); ok {
		return cIdent(x.Name), t
	}
	if _, ok := fb.cg.globalDecls[x.Name]; ok {
		if t, ok2 := fb.cg.globalScope.lookup(x.Name); ok2 {
			return "g_" + x.Name, t
		}
		panic(fmt.Sprintf("nox: global 'let %s' is referenced before it is defined", x.Name))
	}
	panic(fmt.Sprintf("nox: %s: undefined name '%s'", fb.fname, x.Name))
}

// ---------------- arrays ----------------

func (fb *funcBuilder) genArrayLit(c *ctx, x *ast.ArrayLit) (string, Type) {
	tmp := fb.cg.freshTmp("arr")
	c.emit(compilef("nox_array %s = nox_array_new();", tmp))
	var elemType *Type
	for _, el := range x.Elems {
		code, t := fb.genExpr(c, el)
		if elemType == nil {
			et := t
			elemType = &et
		} else if !elemType.Equals(t) {
			panic(fmt.Sprintf("nox: %s: array literal has mixed element types (%s vs %s)", fb.fname, elemType.String(), t.String()))
		}
		etmp := fb.cg.freshTmp("elem")
		c.emit(compilef("%s %s = %s;", fb.cg.ctype(t), etmp, code))
		c.emit(compilef("nox_array_push_raw(&%s, &%s, sizeof(%s));", tmp, etmp, fb.cg.ctype(t)))
	}
	if elemType == nil {
		return tmp, TArray(Type{Kind: KUnknown})
	}
	return tmp, TArray(*elemType)
}

// ---------------- binary / unary ----------------

func (fb *funcBuilder) genBinaryExpr(c *ctx, x *ast.BinaryExpr) (string, Type) {
	if x.Op == token.AND || x.Op == token.OR {
		return fb.genShortCircuit(c, x)
	}
	// `x == null` / `x != null`: null cannot appear as a general expression
	// (Nox forbids explicitly assigning it), but comparing a pointer or
	// class value against null — e.g. to check an array `.find(...)` result
	// — is common and necessary, so it's handled here as a special case
	// rather than through the generic NullLit codegen path.
	if (x.Op == token.EQ || x.Op == token.NE) && (isNullLit(x.X) || isNullLit(x.Y)) {
		return fb.genNullComparison(c, x)
	}
	lc, lt := fb.genExpr(c, x.X)
	rc, rt := fb.genExpr(c, x.Y)

	switch x.Op {
	case token.PLUS:
		if lt.Kind == KString && rt.Kind == KString {
			return fmt.Sprintf("nox_string_concat(%s, %s)", lc, rc), TString()
		}
		requireSameNumeric(fb.fname, lt, rt)
		return fmt.Sprintf("(%s + %s)", lc, rc), lt
	case token.MINUS:
		requireSameNumeric(fb.fname, lt, rt)
		return fmt.Sprintf("(%s - %s)", lc, rc), lt
	case token.STAR:
		requireSameNumeric(fb.fname, lt, rt)
		return fmt.Sprintf("(%s * %s)", lc, rc), lt
	case token.SLASH:
		requireSameNumeric(fb.fname, lt, rt)
		return fmt.Sprintf("(%s / %s)", lc, rc), lt
	case token.PERCENT:
		if lt.Kind != KInt || rt.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: '%%' requires int operands", fb.fname))
		}
		return fmt.Sprintf("(%s %% %s)", lc, rc), TInt()
	case token.LT, token.GT, token.LE, token.GE:
		if lt.Kind == KString && rt.Kind == KString {
			op := map[token.Kind]string{token.LT: "<", token.GT: ">", token.LE: "<=", token.GE: ">="}[x.Op]
			return fmt.Sprintf("(nox_string_cmp(%s, %s) %s 0)", lc, rc, op), TBool()
		}
		requireSameNumeric(fb.fname, lt, rt)
		return fmt.Sprintf("(%s %s %s)", lc, x.Op.String(), rc), TBool()
	case token.EQ, token.NE:
		if !lt.Equals(rt) {
			panic(fmt.Sprintf("nox: %s: cannot compare %s with %s", fb.fname, lt.String(), rt.String()))
		}
		neg := ""
		if x.Op == token.NE {
			neg = "!"
		}
		if lt.Kind == KString {
			return fmt.Sprintf("(%snox_string_eq(%s, %s))", neg, lc, rc), TBool()
		}
		return fmt.Sprintf("(%s %s %s)", lc, x.Op.String(), rc), TBool()
	case token.AMP, token.PIPE, token.CARET:
		if lt.Kind != KInt || rt.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: bitwise operators require int operands", fb.fname))
		}
		return fmt.Sprintf("(%s %s %s)", lc, x.Op.String(), rc), TInt()
	}
	panic(fmt.Sprintf("nox: %s: unhandled binary operator %s", fb.fname, x.Op.String()))
}

func requireSameNumeric(fname string, lt, rt Type) {
	if (lt.Kind != KInt && lt.Kind != KFloat) || !lt.Equals(rt) {
		panic(fmt.Sprintf("nox: %s: arithmetic requires two operands of the same numeric type (got %s and %s); Nox performs no implicit conversion (use .toFloat()/.toInt())", fname, lt.String(), rt.String()))
	}
}

func isNullLit(e ast.Expr) bool {
	_, ok := e.(*ast.NullLit)
	return ok
}

func (fb *funcBuilder) genNullComparison(c *ctx, x *ast.BinaryExpr) (string, Type) {
	valExpr := x.X
	if isNullLit(x.X) {
		valExpr = x.Y
	}
	code, t := fb.genExpr(c, valExpr)
	if t.Kind != KPointer && t.Kind != KClass {
		panic(fmt.Sprintf("nox: %s: 'null' can only be compared against a pointer or class value, not %s", fb.fname, t.String()))
	}
	op := "=="
	if x.Op == token.NE {
		op = "!="
	}
	return fmt.Sprintf("(%s %s NULL)", code, op), TBool()
}

