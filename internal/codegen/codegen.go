package codegen

import (
	"fmt"
	"sort"
	"strings"

	"nox/internal/ast"
)

// Namespace groups declarations reachable through a `::` path (either a
// stdlib package, a `include`d C header, or an imported Nox source file).
type Namespace struct {
	Kind    NamespaceKind
	Funcs   map[string]*ast.FuncDecl
	Classes map[string]*ast.ClassDecl
	Globals map[string]*ast.LetStmt
	CPrefix string // for KindInclude: how to call into C, e.g. "" (call bare name)

	// namespaceKeyPrefix is set transiently by resolveNamespace to the
	// joined "::" path that matched, for building instantiation cache keys
	// that don't collide between same-named functions in different
	// namespaces.
	namespaceKeyPrefix string
}

type NamespaceKind int

const (
	NSStdlib NamespaceKind = iota
	NSInclude
	NSUser
)

// Scope is a lexical block scope mapping Nox variable names to their type.
type Scope struct {
	vars   map[string]Type
	parent *Scope
}

func newScope(parent *Scope) *Scope {
	return &Scope{vars: map[string]Type{}, parent: parent}
}

func (s *Scope) define(name string, t Type) { s.vars[name] = t }

