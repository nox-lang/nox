package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func findMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name && !m.IsStatic {
			return m
		}
	}
	return nil
}

func findStaticMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name && m.IsStatic {
			return m
