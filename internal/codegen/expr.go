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

// genShortCircuit implements `&&`/`||` while preserving short-circuit
// evaluation even when the right-hand operand requires hoisted
// pre-statements (e.g. it contains a `?` propagation or an await).
func (fb *funcBuilder) genShortCircuit(c *ctx, x *ast.BinaryExpr) (string, Type) {
	lcCode, lt := fb.genExpr(c, x.X)
	if lt.Kind != KBool {
		panic(fmt.Sprintf("nox: %s: '%s' requires bool operands", fb.fname, x.Op.String()))
	}
	rc2, rpre := newCtx(c.scope)
	rcCode, rt := fb.genExpr(rc2, x.Y)
	if rt.Kind != KBool {
		panic(fmt.Sprintf("nox: %s: '%s' requires bool operands", fb.fname, x.Op.String()))
	}
	if len(*rpre) == 0 {
		op := "&&"
		if x.Op == token.OR {
			op = "||"
		}
		return fmt.Sprintf("(%s %s %s)", lcCode, op, rcCode), TBool()
	}
	tmp := fb.cg.freshTmp("sc")
	var elseBranch strings.Builder
	for _, p := range *rpre {
		elseBranch.WriteString(p)
	}
	elseBranch.WriteString(compilef("%s = %s;", tmp, rcCode))
	c.emit(compilef("bool %s;", tmp))
	if x.Op == token.AND {
		c.emit(compilef("if (!(%s)) { %s = false; } else {", lcCode, tmp))
	} else {
		c.emit(compilef("if (%s) { %s = true; } else {", lcCode, tmp))
	}
	c.emit(indent(elseBranch.String(), "    "))
	c.emit("}\n")
	return tmp, TBool()
}

func (fb *funcBuilder) genUnaryExpr(c *ctx, x *ast.UnaryExpr) (string, Type) {
	switch x.Op {
	case token.MINUS:
		code, t := fb.genExpr(c, x.X)
		if t.Kind != KInt && t.Kind != KFloat {
			panic(fmt.Sprintf("nox: %s: unary '-' requires a numeric operand", fb.fname))
		}
		return fmt.Sprintf("(-%s)", code), t
	case token.NOT:
		code, t := fb.genExpr(c, x.X)
		if t.Kind != KBool {
			panic(fmt.Sprintf("nox: %s: unary '!' requires a bool operand", fb.fname))
		}
		return fmt.Sprintf("(!%s)", code), TBool()
	case token.AMP:
		lvalue, t := fb.genLvalue(c, x.X)
		return fmt.Sprintf("(&%s)", lvalue), TPointer(t)
	case token.STAR:
		code, t := fb.genExpr(c, x.X)
		if t.Kind != KPointer {
			panic(fmt.Sprintf("nox: %s: unary '*' requires a pointer operand", fb.fname))
		}
		return fmt.Sprintf("(*%s)", code), *t.Elem
	}
	panic(fmt.Sprintf("nox: %s: unhandled unary operator", fb.fname))
}

// genLvalue returns a C lvalue expression (suitable for `&`) for the subset
// of expressions that denote a storage location: identifiers, array
// indexing, and class member access.
func (fb *funcBuilder) genLvalue(c *ctx, e ast.Expr) (string, Type) {
	switch x := e.(type) {
	case *ast.Ident:
		return fb.genIdent(c, x)
	case *ast.IndexExpr:
		xCode, xType := fb.genExpr(c, x.X)
		idxCode, idxType := fb.genExpr(c, x.Index)
		switch xType.Kind {
		case KSlice:
			c.emit(compilef("nox_slice_check_index(&(%s), %s);", xCode, idxCode))
			elemC := fb.cg.ctype(*xType.Elem)
			return fmt.Sprintf("((%s*)(%s).data)[%s]", elemC, xCode, idxCode), *xType.Elem
		case KArray:
			if idxType.Kind != KInt {
				panic(fmt.Sprintf("nox: %s: array index must be int", fb.fname))
			}
			if _, isCall := x.X.(*ast.CallExpr); isCall {
				panic(fmt.Sprintf("nox: %s: cannot assign into an element of an array returned by a function call (arrays are copied by value)", fb.fname))
			}
			c.emit(compilef("nox_check_array_index(%s, %dLL);", idxCode, xType.Len))
			return fmt.Sprintf("(%s).d[%s]", xCode, idxCode), *xType.Elem
		case KMap:
			if !idxType.Equals(*xType.Key) {
				panic(fmt.Sprintf("nox: %s: map key has type %s, expected %s", fb.fname, idxType.String(), xType.Key.String()))
			}
			keyTmp := fb.cg.freshTmp("mkey")
			c.emit(compilef("%s %s = %s;", fb.cg.ctype(*xType.Key), keyTmp, idxCode))
			return fmt.Sprintf("(*(%s*)nox_map_put(%s, &%s))", fb.cg.ctype(*xType.Elem), xCode, keyTmp), *xType.Elem
		}
		panic(fmt.Sprintf("nox: %s: indexing requires a slice, array, or map, got %s", fb.fname, xType.String()))
	case *ast.MemberExpr:
		xCode, xType := fb.genExpr(c, x.X)
		if xType.Kind != KClass {
			panic(fmt.Sprintf("nox: %s: cannot take the address of a member of a non-class value", fb.fname))
		}
		ci := fb.cg.classInstances[xType.ClassKey]
		ft, ok := ci.FieldTypes[x.Name]
		if !ok {
			panic(fmt.Sprintf("nox: %s: class '%s' has no field '%s'", fb.fname, ci.ClassName, x.Name))
		}
		if fieldIsPrivate(ci.Decl, x.Name) && fb.currentClassKey != xType.ClassKey {
			panic(fmt.Sprintf("nox: %s: '%s' is a private field of class '%s'", fb.fname, x.Name, ci.ClassName))
		}
		return fmt.Sprintf("(%s)->%s", xCode, x.Name), ft
	}
	panic(fmt.Sprintf("nox: %s: expression is not addressable", fb.fname))
}

