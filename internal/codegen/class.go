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

// ---------------- static class members ----------------
// A static field/method belongs to the class's NAME, not to any one
// monomorphized ClassInstance: `Counter.total` is one shared int no matter
// how many different field-type shapes Counter.new(...) has produced
// elsewhere in the program. Static fields are therefore registered once,
// eagerly, from every class declaration the compiler knows about (see
// registerStaticFields, called from NewCodegen and resolveImports) rather
// than lazily on first `.new()` the way instance fields are.

// registerStaticFields records the static fields of one class declaration.
// A static field must carry an explicit type (unlike an instance field, it
// has no constructor call to infer one from).
func (cg *Codegen) registerStaticFields(className string, decl *ast.ClassDecl) {
	for _, f := range decl.Fields {
		if !f.IsStatic {
			continue
		}
