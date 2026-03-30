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
		if recvIdent, ok := callee.X.(*ast.Ident); ok {
			if _, isLocal := c.scope.lookup(recvIdent.Name); !isLocal {
				if recvIdent.Name == "Thread" || recvIdent.Name == "Task" {
					return fb.genStaticCall(c, recvIdent.Name, callee.Name, x.Args)
				}
				if _, isClass := fb.cg.classesByName[recvIdent.Name]; isClass && callee.Name != "new" {
					return fb.genStaticCall(c, recvIdent.Name, callee.Name, x.Args)
				}
			}
		}
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
		if xType.Kind == KThread {
			return fb.genThreadMethod(c, xCode, callee.Name, x.Args)
		}
		panic(fmt.Sprintf("nox: %s: cannot call '.%s(...)' on a value of type %s", fb.fname, callee.Name, xType.String()))

	case *ast.Ident:
		if _, ok := fb.cg.funcsByName[callee.Name]; ok {
			if _, isLocal := c.scope.lookup(callee.Name); !isLocal {
				return fb.callUserFunc(c, callee.Name, x.Args)
