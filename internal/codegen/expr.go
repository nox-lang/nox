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
	case *ast.SliceExpr:
		return fb.genSliceExpr(c, x)
	case *ast.MapLit:
		return fb.genMapLit(c, x)
	case *ast.MakeExpr:
		return fb.genMakeExpr(c, x)
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
	if x.Type != nil {
		te := fb.cg.resolveTypeExpr(x.Type)
		if te.Kind == KArray {
			return fb.genFixedArrayLit(c, x, te)
		}
		return fb.genTypedSliceLit(c, x, te)
	}
	tmp := fb.cg.freshTmp("arr")
	c.emit(compilef("nox_slice %s = nox_slice_new();", tmp))
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
		c.emit(compilef("nox_slice_push_raw(&%s, &%s, sizeof(%s));", tmp, etmp, fb.cg.ctype(t)))
	}
	if elemType == nil {
		return tmp, TSlice(Type{Kind: KUnknown})
	}
	return tmp, TSlice(*elemType)
}

// genTypedSliceLit compiles the explicit-element-type form `[]T{a, b, c}`.
func (fb *funcBuilder) genTypedSliceLit(c *ctx, x *ast.ArrayLit, sliceType Type) (string, Type) {
	elemType := *sliceType.Elem
	tmp := fb.cg.freshTmp("arr")
	c.emit(compilef("nox_slice %s = nox_slice_new();", tmp))
	for _, el := range x.Elems {
		code, t := fb.genExpr(c, el)
		if !elemType.Equals(t) {
			panic(fmt.Sprintf("nox: %s: []%s{...} element has type %s", fb.fname, elemType.String(), t.String()))
		}
		etmp := fb.cg.freshTmp("elem")
		c.emit(compilef("%s %s = %s;", fb.cg.ctype(elemType), etmp, code))
		c.emit(compilef("nox_slice_push_raw(&%s, &%s, sizeof(%s));", tmp, etmp, fb.cg.ctype(elemType)))
	}
	return tmp, sliceType
}

// genFixedArrayLit compiles the fixed-size form `[N]T{a, b, c}`.
func (fb *funcBuilder) genFixedArrayLit(c *ctx, x *ast.ArrayLit, arrType Type) (string, Type) {
	elemType := *arrType.Elem
	if int64(len(x.Elems)) > arrType.Len {
		panic(fmt.Sprintf("nox: %s: [%d]%s{...} literal has %d elements, more than its length", fb.fname, arrType.Len, elemType.String(), len(x.Elems)))
	}
	arrC := fb.cg.ctype(arrType)
	tmp := fb.cg.freshTmp("farr")
	c.emit(compilef("%s %s = {0};", arrC, tmp))
	for i, el := range x.Elems {
		code, t := fb.genExpr(c, el)
		if !elemType.Equals(t) {
			panic(fmt.Sprintf("nox: %s: [%d]%s{...} element has type %s", fb.fname, arrType.Len, elemType.String(), t.String()))
		}
		c.emit(compilef("%s.d[%d] = %s;", tmp, i, code))
	}
	return tmp, arrType
}

// ---------------- maps ----------------

func (fb *funcBuilder) genMapLit(c *ctx, x *ast.MapLit) (string, Type) {
	var keyType, valType Type
	haveType := false
	if x.Type != nil {
		mt := fb.cg.resolveTypeExpr(x.Type)
		keyType, valType = *mt.Key, *mt.Elem
		haveType = true
	}
	var kcodes, vcodes []string
	for i := range x.Keys {
		kc, kt := fb.genExpr(c, x.Keys[i])
		vc, vt := fb.genExpr(c, x.Vals[i])
		if !haveType {
			keyType, valType = kt, vt
			haveType = true
		} else if !keyType.Equals(kt) || !valType.Equals(vt) {
			panic(fmt.Sprintf("nox: %s: map literal entry has type %s: %s, expected %s: %s", fb.fname, kt.String(), vt.String(), keyType.String(), valType.String()))
		}
		kcodes = append(kcodes, kc)
		vcodes = append(vcodes, vc)
	}
	if !haveType {
		panic(fmt.Sprintf("nox: %s: an empty map literal needs an explicit type, e.g. map<string, int>{}", fb.fname))
	}
	if !isValidMapKey(keyType) {
		panic(fmt.Sprintf("nox: %s: %s cannot be used as a map key", fb.fname, keyType.String()))
	}
	mTmp := fb.cg.freshTmp("map")
	c.emit(compilef("nox_map* %s = nox_map_new(%s, sizeof(%s), sizeof(%s));", mTmp, mapKeyKindC(keyType), fb.cg.ctype(keyType), fb.cg.ctype(valType)))
	for i := range kcodes {
		kTmp := fb.cg.freshTmp("mk")
		c.emit(compilef("%s %s = %s;", fb.cg.ctype(keyType), kTmp, kcodes[i]))
		c.emit(compilef("*(%s*)nox_map_put(%s, &%s) = %s;", fb.cg.ctype(valType), mTmp, kTmp, vcodes[i]))
	}
	return mTmp, TMap(keyType, valType)
}

// ---------------- make() ----------------

func (fb *funcBuilder) genMakeExpr(c *ctx, x *ast.MakeExpr) (string, Type) {
	t := fb.cg.resolveTypeExpr(x.Type)
	switch t.Kind {
	case KSlice:
		if len(x.Args) < 1 || len(x.Args) > 2 {
			panic(fmt.Sprintf("nox: %s: make([]T, len[, cap]) takes a length and an optional capacity", fb.fname))
		}
		lenCode, lt := fb.genExpr(c, x.Args[0])
		if lt.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: make([]T, len): len must be int", fb.fname))
		}
		capCode := lenCode
		if len(x.Args) == 2 {
			cc, ct := fb.genExpr(c, x.Args[1])
			if ct.Kind != KInt {
				panic(fmt.Sprintf("nox: %s: make([]T, len, cap): cap must be int", fb.fname))
			}
			capCode = cc
		}
		elemC := fb.cg.ctype(*t.Elem)
		tmp := fb.cg.freshTmp("mkslice")
		c.emit(compilef("nox_slice %s = nox_slice_make(%s, %s, sizeof(%s));", tmp, lenCode, capCode, elemC))
		return tmp, t
	case KMap:
		if len(x.Args) != 0 {
			panic(fmt.Sprintf("nox: %s: make(map<K, V>) takes no extra arguments", fb.fname))
		}
		tmp := fb.cg.freshTmp("mkmap")
		c.emit(compilef("nox_map* %s = nox_map_new(%s, sizeof(%s), sizeof(%s));", tmp, mapKeyKindC(*t.Key), fb.cg.ctype(*t.Key), fb.cg.ctype(*t.Elem)))
		return tmp, t
	}
	panic(fmt.Sprintf("nox: %s: make(...) supports slice and map types only, got %s", fb.fname, t.String()))
}

// ---------------- slicing: x[lo:hi] ----------------

func (fb *funcBuilder) genSliceExpr(c *ctx, x *ast.SliceExpr) (string, Type) {
	xCode, xType := fb.genExpr(c, x.X)
	evalBound := func(e ast.Expr, def string) string {
		if e == nil {
			return def
		}
		code, t := fb.genExpr(c, e)
		if t.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: slice bounds must be int", fb.fname))
		}
		return code
	}
	if xType.Kind == KString {
		loCode := evalBound(x.Lo, "0LL")
		hiCode := evalBound(x.Hi, fmt.Sprintf("((int64_t)(%s).len)", xCode))
		return fmt.Sprintf("nox_string_substring(%s, %s, %s)", xCode, loCode, hiCode), TString()
	}
	if xType.Kind != KSlice {
		panic(fmt.Sprintf("nox: %s: 'x[lo:hi]' requires a slice or string, got %s", fb.fname, xType.String()))
	}
	tmp := fb.cg.freshTmp("sl")
	c.emit(compilef("nox_slice %s = %s;", tmp, xCode))
	loCode := evalBound(x.Lo, "0LL")
	hiCode := evalBound(x.Hi, fmt.Sprintf("%s.len", tmp))
	elemC := fb.cg.ctype(*xType.Elem)
	return fmt.Sprintf("nox_slice_slice(%s, %s, %s, sizeof(%s))", tmp, loCode, hiCode, elemC), xType
}

// ---------------- binary / unary ----------------

func (fb *funcBuilder) genBinaryExpr(c *ctx, x *ast.BinaryExpr) (string, Type) {
	if x.Op == token.AND || x.Op == token.OR {
		return fb.genShortCircuit(c, x)
	}
