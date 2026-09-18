package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func (fb *funcBuilder) genCallExpr(c *ctx, x *ast.CallExpr) (string, Type) {
	switch callee := x.Callee.(type) {
	case *ast.QualIdent:
		return fb.genQualIdentCall(c, callee, x.Args)

	case *ast.MemberExpr:
		if recvIdent, ok := callee.X.(*ast.Ident); ok && callee.Name == "new" {
			if _, isClass := fb.cg.classesByName[recvIdent.Name]; isClass {
				return fb.genClassNew(c, recvIdent.Name, x.Args)
			}
		}
		if callee.Name == "delete" {
			recvVar, recvType := fb.genReceiverLvalue(c, callee.X)
			return fb.genDeleteMethod(c, recvVar, recvType, x.Args)
		}
		if mutatingBuiltinMethods[callee.Name] {
			// These need a genuine addressable receiver so the mutation is
			// observable on the underlying storage, not a throwaway copy.
			recvVar, recvType := fb.genReceiverLvalue(c, callee.X)
			return fb.genBuiltinMethodCall(c, recvVar, recvType, callee.Name, x.Args)
		}
		if builtinMethodNames[callee.Name] {
			xCode, xType := fb.genExpr(c, callee.X)
			return fb.genBuiltinMethodCall(c, xCode, xType, callee.Name, x.Args)
		}
		xCode, xType := fb.genExpr(c, callee.X)
		if xType.Kind == KClass {
			return fb.genMethodCall(c, xCode, xType, callee.Name, x.Args)
		}
		panic(fmt.Sprintf("nox: %s: cannot call '.%s(...)' on a value of type %s", fb.fname, callee.Name, xType.String()))

	case *ast.Ident:
		if _, ok := fb.cg.funcsByName[callee.Name]; ok {
			if _, isLocal := c.scope.lookup(callee.Name); !isLocal {
				return fb.callUserFunc(c, callee.Name, x.Args)
			}
		}
		if t, ok := c.scope.lookup(callee.Name); ok && t.Kind == KFunc {
			return fb.genClosureCall(c, cIdent(callee.Name), t, x.Args)
		}
		panic(fmt.Sprintf("nox: %s: call to undefined function '%s'", fb.fname, callee.Name))
	}
	panic(fmt.Sprintf("nox: %s: expression is not callable", fb.fname))
}

// genDeleteMethod implements `.delete()`, available on strings, arrays,
// class instances, and pointers: it explicitly frees the underlying
// GC-managed memory right now rather than waiting for the collector, and
// clears the (necessarily addressable — see genReceiverLvalue) receiver so
// it can't be read back accidentally. This is a deliberate escape hatch
// from automatic memory management, not something normal Nox code needs;
// using the value again afterwards is undefined behavior, exactly like a
// manual free() in C.
func (fb *funcBuilder) genDeleteMethod(c *ctx, recv string, recvType Type, args []ast.Expr) (string, Type) {
	if len(args) != 0 {
		panic(fmt.Sprintf("nox: %s: '.delete()' takes no arguments", fb.fname))
	}
	switch recvType.Kind {
	case KString:
		c.emit(compilef("if ((%s).data) { GC_FREE((%s).data); }", recv, recv))
		c.emit(compilef("%s.data = NULL; %s.len = 0;", recv, recv))
	case KArray:
		c.emit(compilef("if ((%s).data) { GC_FREE((%s).data); }", recv, recv))
		c.emit(compilef("%s.data = NULL; %s.len = 0; %s.cap = 0;", recv, recv, recv))
	case KClass, KPointer:
		c.emit(compilef("if (%s) { GC_FREE(%s); }", recv, recv))
		c.emit(compilef("%s = NULL;", recv))
	default:
		panic(fmt.Sprintf("nox: %s: '.delete()' is not available on type %s", fb.fname, recvType.String()))
	}
	return "", TVoid()
}

// genClosureCall invokes a first-class function value through its
// {fn, env} fat-pointer representation.
func (fb *funcBuilder) genClosureCall(c *ctx, closureCode string, t Type, args []ast.Expr) (string, Type) {
	if len(args) != len(t.Params) {
		panic(fmt.Sprintf("nox: %s: closure call expects %d argument(s), got %d", fb.fname, len(t.Params), len(args)))
	}
	tmp := fb.cg.freshTmp("cloval")
	c.emit(compilef("%s %s = %s;", fb.cg.ctype(t), tmp, closureCode))
	var argCodes []string
	for i, a := range args {
		code, at := fb.genExpr(c, a)
		if !at.Equals(t.Params[i]) {
			panic(fmt.Sprintf("nox: %s: closure argument %d: expected %s, got %s", fb.fname, i+1, t.Params[i].String(), at.String()))
		}
		argCodes = append(argCodes, code)
	}
	callArgs := append([]string{tmp + ".env"}, argCodes...)
	call := fmt.Sprintf("%s.fn(%s)", tmp, strings.Join(callArgs, ", "))
	ret := TVoid()
	if t.Ret != nil {
		ret = *t.Ret
	}
	return call, ret
}

