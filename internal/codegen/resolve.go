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

func (cg *Codegen) resolveTypeExprDepth(te *ast.TypeExpr, depth int) Type {
	if depth > maxAliasDepth {
		panic(fmt.Sprintf("nox: type '%s' is defined in terms of itself", te.Name))
	}
	rec := func(t *ast.TypeExpr) Type { return cg.resolveTypeExprDepth(t, depth+1) }
	switch te.Name {
	case "int":
		return TInt()
	case "float":
		return TFloat()
	case "bool":
		return TBool()
	case "string":
		return TString()
	case "slice":
		return TSlice(rec(te.Elem))
	case "array":
		if te.Len < 0 {
			panic("nox: array length must not be negative")
		}
		if te.Elem == nil || te.Elem.Name == "" {
			panic("nox: array type requires an element type, e.g. [3]int")
		}
		return TArrayN(rec(te.Elem), te.Len)
	case "map":
		if te.Key == nil || te.Elem == nil {
			panic("nox: 'map' type requires key and value types, e.g. map<string, int>")
		}
		k, v := rec(te.Key), rec(te.Elem)
		if !isValidMapKey(k) {
			panic(fmt.Sprintf("nox: %s cannot be used as a map key (use int, string, bool, or a class/pointer)", k.String()))
		}
