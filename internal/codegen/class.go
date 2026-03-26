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
		}
	}
	return nil
}

func fieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name && !f.IsStatic {
			return f
		}
	}
	return nil
}

func staticFieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name && f.IsStatic {
			return f
		}
	}
	return nil
}

// A member (field or method) whose name does not start with an uppercase
// letter is only visible from inside its own class's own methods — the
// replacement for the removed `private` keyword (see resolve.go's
// isExported, which applies the same rule to package-level declarations).
func fieldIsPrivate(decl *ast.ClassDecl, name string) bool {
	f := fieldDecl(decl, name)
	return f != nil && !isExported(name)
}

func classCacheKey(className, argsKey string) string { return className + "#" + argsKey }

