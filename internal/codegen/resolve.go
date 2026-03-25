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

