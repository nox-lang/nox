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

// ClassInstance is one monomorphized specialization of a Nox class.
type ClassInstance struct {
	ClassKey      string
	ClassName     string
	Decl          *ast.ClassDecl
	FieldTypes    map[string]Type
	FieldOrder    []string
	StructEmitted bool
	StructC       string
	NewFuncName   string
	Methods       map[string]*FuncInstance // key: methodName + "#" + arg-type mangle
}

// FuncSig identifies a requested instantiation.
type funcKey struct {
	name    string
	argsKey string
}

type Codegen struct {
	file *ast.File

	funcsByName   map[string]*ast.FuncDecl
	classesByName map[string]*ast.ClassDecl
	globalDecls   map[string]*ast.LetStmt

	namespaces map[string]*Namespace // key: joined "::" path, e.g. "io", "libs::math"

	globalScope *Scope
	globalInitC []string // statements to run in nox_init_globals()

	funcInstances map[string]*FuncInstance // key: mangled name
	funcOrder     []string
	instCache     map[funcKey]*FuncInstance

	classInstances map[string]*ClassInstance // key: ClassKey
	classOrder     []string
	classCache     map[string]*ClassInstance // key: className + argsKey

	closureTypes map[string]bool
	taskTypes    map[string]bool
	typeDefs     []string

	includeHeaders []string // "stdio.h" etc, deduped
	includeSeen    map[string]bool

	tmpCounter  int
	nameCounter int

	errStack int // nesting depth counter for generating unique labels (not currently required, reserved)

	warnings []string
}

func NewCodegen(file *ast.File) *Codegen {
	cg := &Codegen{
		file:           file,
		funcsByName:    map[string]*ast.FuncDecl{},
		classesByName:  map[string]*ast.ClassDecl{},
		globalDecls:    map[string]*ast.LetStmt{},
		namespaces:     map[string]*Namespace{},
		funcInstances:  map[string]*FuncInstance{},
		instCache:      map[funcKey]*FuncInstance{},
		classInstances: map[string]*ClassInstance{},
		classCache:     map[string]*ClassInstance{},
		closureTypes:   map[string]bool{},
		taskTypes:      map[string]bool{},
		includeSeen:    map[string]bool{},
	}
	cg.globalScope = newScope(nil)
	for _, fn := range file.Funcs {
		cg.funcsByName[fn.Name] = fn
	}
	for _, c := range file.Classes {
		cg.classesByName[c.Name] = c
	}
	for _, g := range file.Globals {
		cg.globalDecls[g.Name] = g
	}
	cg.registerStdlibNamespaces()
	for _, inc := range file.Includes {
		alias := inc.Alias
		if alias == "" {
			alias = headerStem(inc.Header)
		}
		cg.includeHeaderNamed(inc.Header)
		cg.namespaces[alias] = &Namespace{Kind: NSInclude}
	}
	return cg
}

func headerStem(h string) string {
	h = strings.TrimSuffix(h, ".h")
	if i := strings.LastIndexByte(h, '/'); i >= 0 {
		h = h[i+1:]
	}
	return h
}

func (cg *Codegen) includeHeaderNamed(h string) {
	if cg.includeSeen[h] {
		return
	}
	cg.includeSeen[h] = true
	cg.includeHeaders = append(cg.includeHeaders, h)
}

func (cg *Codegen) freshTmp(prefix string) string {
	cg.tmpCounter++
	return fmt.Sprintf("__%s%d", prefix, cg.tmpCounter)
}

