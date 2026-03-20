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

func (s *Scope) lookup(name string) (Type, bool) {
	for cur := s; cur != nil; cur = cur.parent {
		if t, ok := cur.vars[name]; ok {
			return t, true
		}
	}
	return Type{}, false
}

// FuncInstance is one monomorphized specialization of a Nox function.
type FuncInstance struct {
	MangledName  string
	Decl         *ast.FuncDecl
	ParamTypes   []Type
	RetType      Type
	RetTypeKnown bool
	IsAsync      bool
	Emitting     bool // guards against infinite recursion while generating
	Emitted      bool
	Forward      string // forward declaration line
	Body         string // full definition (emitted once ready)

	// Async-only: names of the auxiliary synchronous body function and the
	// pthread trampoline generated alongside the public task-returning entry
	// point (see emitAsyncFunc).
	AsyncBodyName   string
	AsyncThreadName string
}

