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

