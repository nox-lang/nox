package codegen

import (
	"fmt"
	"unicode"
	"unicode/utf8"

	"nox/internal/ast"
)

// isExported reports whether a Nox name is public: like Go, a name is
// exported exactly when it starts with an uppercase letter. Everything else
// is package-private (top-level declarations) or class-private (members).
func isExported(name string) bool {
	r, _ := utf8.DecodeRuneInString(name)
	return unicode.IsUpper(r)
}

// lowerFirst / upperFirst adjust the first letter of a name. They are used so
// that the built-in members of string / slice / array / map values (which are
// part of the language rather than a package) can be written either way:
// `xs.push(1)` and `xs.Push(1)` are the same method.
func lowerFirst(s string) string {
	if s == "" {
		return s
	}
	r, n := utf8.DecodeRuneInString(s)
	return string(unicode.ToLower(r)) + s[n:]
}

const maxAliasDepth = 32

// resolveTypeExpr turns a parsed type annotation into a concrete codegen
// Type. Built-in kinds (int/float/bool/string/[]T/[N]T/map<K,V>/pointer<T>/
// Task<T>/Thread/func(...)) always resolve, and so does any `type` alias. A
// bare class name resolves only if that class has already been instantiated
// somewhere in the program (Nox classes are monomorphized like functions; see
// class.go), which covers the common case of explicit class-typed
// parameters/locals following at least one `ClassName.new(...)` call earlier
// in the program.
func (cg *Codegen) resolveTypeExpr(te *ast.TypeExpr) Type {
	return cg.resolveTypeExprDepth(te, 0)
}

