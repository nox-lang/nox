package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func findMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name {
			return m
		}
	}
	return nil
}

func fieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name {
			return f
		}
	}
	return nil
}

