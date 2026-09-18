package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

var builtinMethodNames = map[string]bool{
	"push": true, "pop": true, "insert": true, "remove": true, "clear": true,
	"each": true, "eachIndex": true, "eachLine": true,
	"map": true, "filter": true, "find": true, "sort": true, "reverse": true,
	"contains": true, "startsWith": true, "endsWith": true, "substring": true, "empty": true,
	"toInt": true, "toFloat": true, "toBool": true, "toString": true,
}

// mutatingBuiltinMethods are the built-ins that need a genuinely addressable
// receiver (see genReceiverLvalue) because they modify the receiver's
// underlying storage in place.
var mutatingBuiltinMethods = map[string]bool{
	"push": true, "pop": true, "insert": true, "remove": true, "clear": true, "sort": true,
}

// genReceiverLvalue evaluates e as the receiver of a built-in method call.
// If e naturally denotes storage (a variable, an index, or a class member),
// the *real* underlying storage is returned so that mutating methods
// (push/pop/insert/remove/clear/sort) affect it in place. Otherwise (e.g.
// the receiver is itself a call expression) the value is hoisted into a
// fresh local, which is still a valid receiver for read-only methods and is
// harmlessly "mutated in isolation" for the others.
func (fb *funcBuilder) genReceiverLvalue(c *ctx, e ast.Expr) (string, Type) {
	switch e.(type) {
	case *ast.Ident, *ast.IndexExpr, *ast.MemberExpr:
		return fb.genLvalue(c, e)
	}
	code, t := fb.genExpr(c, e)
	tmp := fb.cg.freshTmp("recv")
	c.emit(compilef("%s %s = %s;", fb.cg.ctype(t), tmp, code))
	return tmp, t
}

func (fb *funcBuilder) genBuiltinMethodCall(c *ctx, recv string, recvType Type, method string, args []ast.Expr) (string, Type) {
	switch method {
	case "toInt", "toFloat", "toBool", "toString":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: '.%s()' takes no arguments", fb.fname, method))
		}
		return fb.genConversion(recv, recvType, method)
	}
	switch recvType.Kind {
	case KString:
		return fb.genStringMethod(c, recv, method, args)
	case KArray:
		return fb.genArrayMethod(c, recv, recvType, method, args)
	}
	panic(fmt.Sprintf("nox: %s: '.%s(...)' is not available on type %s", fb.fname, method, recvType.String()))
}

func (fb *funcBuilder) genConversion(recv string, t Type, method string) (string, Type) {
	conv := func(fn string, rt Type) (string, Type) {
		if fn == "" {
			return recv, rt
		}
		return fmt.Sprintf("%s(%s)", fn, recv), rt
	}
	switch method {
	case "toInt":
		switch t.Kind {
		case KInt:
			return conv("", TInt())
		case KFloat:
			return conv("nox_float_to_int", TInt())
		case KBool:
			return conv("nox_bool_to_int", TInt())
		case KString:
			return conv("nox_string_to_int", TInt())
		}
	case "toFloat":
		switch t.Kind {
		case KFloat:
			return conv("", TFloat())
		case KInt:
			return conv("nox_int_to_float", TFloat())
		case KBool:
			return conv("nox_bool_to_float", TFloat())
		case KString:
			return conv("nox_string_to_float", TFloat())
		}
	case "toBool":
		switch t.Kind {
		case KBool:
			return conv("", TBool())
		case KInt:
			return conv("nox_int_to_bool", TBool())
		case KFloat:
			return conv("nox_float_to_bool", TBool())
		case KString:
			return conv("nox_string_to_bool", TBool())
		}
	case "toString":
		switch t.Kind {
		case KString:
			return conv("", TString())
		case KInt:
			return conv("nox_int_to_string", TString())
		case KFloat:
			return conv("nox_float_to_string", TString())
		case KBool:
			return conv("nox_bool_to_string", TString())
		}
	}
	panic(fmt.Sprintf("nox: %s: '.%s()' is not available on type %s", fb.fname, method, t.String()))
}

// ---------------- string methods ----------------

func (fb *funcBuilder) genStringMethod(c *ctx, recv string, method string, args []ast.Expr) (string, Type) {
	switch method {
	case "empty":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: '.empty()' takes no arguments", fb.fname))
		}
		return fmt.Sprintf("((%s).len == 0)", recv), TBool()
	case "contains", "startsWith", "endsWith":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.%s(...)' takes exactly one argument", fb.fname, method))
		}
		argCode, argType := fb.genExpr(c, args[0])
		if argType.Kind != KString {
			panic(fmt.Sprintf("nox: %s: '.%s(...)' expects a string argument", fb.fname, method))
		}
		fn := map[string]string{"contains": "nox_string_contains", "startsWith": "nox_string_starts_with", "endsWith": "nox_string_ends_with"}[method]
		return fmt.Sprintf("%s(%s, %s)", fn, recv, argCode), TBool()
	case "substring":
		if len(args) != 2 {
			panic(fmt.Sprintf("nox: %s: '.substring(start, end)' takes exactly two arguments", fb.fname))
		}
		a0, t0 := fb.genExpr(c, args[0])
		a1, t1 := fb.genExpr(c, args[1])
		if t0.Kind != KInt || t1.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: '.substring(start, end)' expects int arguments", fb.fname))
		}
		return fmt.Sprintf("nox_string_substring(%s, %s, %s)", recv, a0, a1), TString()
	case "eachLine":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.eachLine(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "eachLine")
		linesTmp := fb.cg.freshTmp("lines")
		c.emit(compilef("nox_array %s = nox_string_split_lines(%s);", linesTmp, recv))
		return fb.genEachLoop(c, linesTmp, TString(), fl, false)
	}
	panic(fmt.Sprintf("nox: %s: string has no method '.%s(...)'", fb.fname, method))
}

// ---------------- array methods ----------------

func (fb *funcBuilder) genArrayMethod(c *ctx, recv string, recvType Type, method string, args []ast.Expr) (string, Type) {
	elemType := *recvType.Elem
	elemC := fb.cg.ctype(elemType)
	switch method {
	case "push":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.push(...)' takes exactly one argument", fb.fname))
		}
		code, t := fb.genExpr(c, args[0])
		if !t.Equals(elemType) {
			panic(fmt.Sprintf("nox: %s: '.push(...)': expected %s, got %s", fb.fname, elemType.String(), t.String()))
		}
		etmp := fb.cg.freshTmp("e")
		c.emit(compilef("%s %s = %s;", elemC, etmp, code))
		c.emit(compilef("nox_array_push_raw(&%s, &%s, sizeof(%s));", recv, etmp, elemC))
		return "", TVoid()
	case "pop":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: '.pop()' takes no arguments", fb.fname))
		}
		tmp := fb.cg.freshTmp("popped")
		c.emit(compilef("%s %s;", elemC, tmp))
		c.emit(compilef("nox_array_pop_raw(&%s, &%s, sizeof(%s));", recv, tmp, elemC))
		return tmp, elemType
	case "insert":
		if len(args) != 2 {
			panic(fmt.Sprintf("nox: %s: '.insert(index, value)' takes exactly two arguments", fb.fname))
		}
		idxCode, idxT := fb.genExpr(c, args[0])
		if idxT.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: '.insert(index, value)': index must be int", fb.fname))
		}
		valCode, valT := fb.genExpr(c, args[1])
		if !valT.Equals(elemType) {
			panic(fmt.Sprintf("nox: %s: '.insert(index, value)': expected %s, got %s", fb.fname, elemType.String(), valT.String()))
		}
		etmp := fb.cg.freshTmp("e")
		c.emit(compilef("%s %s = %s;", elemC, etmp, valCode))
		c.emit(compilef("nox_array_insert_raw(&%s, %s, &%s, sizeof(%s));", recv, idxCode, etmp, elemC))
		return "", TVoid()
	case "remove":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.remove(index)' takes exactly one argument", fb.fname))
		}
		idxCode, idxT := fb.genExpr(c, args[0])
		if idxT.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: '.remove(index)': index must be int", fb.fname))
		}
		c.emit(compilef("nox_array_remove_raw(&%s, %s, sizeof(%s));", recv, idxCode, elemC))
		return "", TVoid()
	case "clear":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: '.clear()' takes no arguments", fb.fname))
		}
		c.emit(compilef("nox_array_clear(&%s);", recv))
		return "", TVoid()
	case "reverse":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: '.reverse()' takes no arguments", fb.fname))
		}
		tmp := fb.cg.freshTmp("rev")
		c.emit(compilef("nox_array %s = nox_array_reverse_raw(%s, sizeof(%s));", tmp, recv, elemC))
		return tmp, recvType
	case "each":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.each(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "each")
		return fb.genEachLoop(c, recv, elemType, fl, false)
	case "eachIndex":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.eachIndex(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "eachIndex")
		return fb.genEachLoop(c, recv, elemType, fl, true)
	case "map":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.map(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "map")
		return fb.genMapLoop(c, recv, elemType, fl)
	case "filter":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.filter(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "filter")
		return fb.genFilterLoop(c, recv, elemType, fl)
	case "find":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: '.find(...)' takes exactly one argument", fb.fname))
		}
		fl := fb.requireFuncLit(args[0], "find")
		return fb.genFindLoop(c, recv, elemType, fl)
	case "sort":
		return fb.genSort(c, recv, elemType, args)
	}
	panic(fmt.Sprintf("nox: %s: array has no method '.%s(...)'", fb.fname, method))
}

func (fb *funcBuilder) requireFuncLit(argExpr ast.Expr, methodName string) *ast.FuncLit {
	fl, ok := argExpr.(*ast.FuncLit)
	if !ok {
		panic(fmt.Sprintf("nox: %s: the callback passed to '.%s(...)' must be a literal anonymous function, e.g. arr.%s((x) { ... })", fb.fname, methodName, methodName))
	}
	return fl
}

type cbParam struct {
	name  string
	typ   Type
	cexpr string
}

// genInlineCallback inlines fl's body with its parameters bound to the given
// C source expressions, in a fresh nested scope. A `yield expr` inside fl's
// body does not return from the enclosing Nox function — it yields a
// per-invocation result (Nox's map/filter/find/each callback semantics),
// available afterwards as (resultVar, resultType) — resultType is nil if
// the body never used `yield`. Unlike `yield`, a `return` inside fl's body
// is an ordinary return from the *enclosing* function, since the callback
// is inlined directly rather than compiled as its own function.
func (fb *funcBuilder) genInlineCallback(scope *Scope, fl *ast.FuncLit, params []cbParam) (bodyC string, resultVar string, resultType *Type) {
	if len(fl.Params) != len(params) {
		panic(fmt.Sprintf("nox: %s: callback expects %d parameter(s), got %d", fb.fname, len(params), len(fl.Params)))
	}
	inner := newScope(scope)
	var sb strings.Builder
	for i, p := range fl.Params {
		inner.define(p.Name, params[i].typ)
		sb.WriteString(compilef("%s %s = %s;", fb.cg.ctype(params[i].typ), cIdent(p.Name), params[i].cexpr))
	}
	resultVar = fb.cg.freshTmp("hofres")
	label := fb.cg.freshTmp("hoflabel")
	sb.WriteString("%%HOFRESDECL%%\n")
	lc := &loopCtx{mode: "hofvalue", resultVar: resultVar, hofLabel: label}
	fb.loopStack = append(fb.loopStack, lc)
	sb.WriteString(fb.genBlock(inner, fl.Body))
	fb.loopStack = fb.loopStack[:len(fb.loopStack)-1]
	sb.WriteString(compilef("%s: ;", label))

	resultType = lc.resultType
	decl := ""
	if resultType != nil {
		decl = fmt.Sprintf("%s %s;", fb.cg.ctype(*resultType), resultVar)
	}
	bodyC = strings.ReplaceAll(sb.String(), "%%HOFRESDECL%%", decl)
	return bodyC, resultVar, resultType
}

func (fb *funcBuilder) genEachLoop(c *ctx, recv string, elemType Type, fl *ast.FuncLit, wantIndex bool) (string, Type) {
	idxVar := fb.cg.freshTmp("i")
	elemC := fb.cg.ctype(elemType)
	var params []cbParam
	if wantIndex {
		if len(fl.Params) != 2 {
			panic(fmt.Sprintf("nox: %s: 'eachIndex' callback needs two parameters (index, value)", fb.fname))
		}
		params = []cbParam{
			{fl.Params[0].Name, TInt(), idxVar},
			{fl.Params[1].Name, elemType, fmt.Sprintf("((%s*)%s.data)[%s]", elemC, recv, idxVar)},
		}
	} else {
		params = []cbParam{{fl.Params[0].Name, elemType, fmt.Sprintf("((%s*)%s.data)[%s]", elemC, recv, idxVar)}}
	}
	bodyC, _, _ := fb.genInlineCallback(c.scope, fl, params)
	loop := fmt.Sprintf("for (int64_t %s = 0; %s < %s.len; %s++) {\n%s}\n", idxVar, idxVar, recv, idxVar, indent(bodyC, "    "))
	c.emit(loop)
	return "", TVoid()
}

