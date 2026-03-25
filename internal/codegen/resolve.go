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
