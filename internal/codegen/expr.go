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

