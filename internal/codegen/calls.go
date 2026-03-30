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
