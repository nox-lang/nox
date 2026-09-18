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

func (cg *Codegen) freshName(prefix string) string {
	cg.nameCounter++
	return fmt.Sprintf("%s_%d", prefix, cg.nameCounter)
}

// Generate compiles the parsed file into a single C source string.
func Generate(file *ast.File, runtimePrelude string, projectRoot string) (out string, err error) {
	defer func() {
		if r := recover(); r != nil {
			if s, ok := r.(string); ok {
				err = fmt.Errorf("%s", s)
				return
			}
			if e, ok := r.(error); ok {
				err = e
				return
			}
			panic(r)
		}
	}()

	cg := NewCodegen(file)
	cg.resolveImports(projectRoot)

	mainDecl, ok := cg.funcsByName["main"]
	if !ok {
		return "", fmt.Errorf("no 'main' function found")
	}

	// Resolve global (top-level) let declarations' types eagerly using only
	// literal-ish initializers; more complex globals are resolved lazily on
	// first reference from within a function body.
	cg.prepassGlobals()

	// Determine main's parameter convention: func main() or func main(args).
	var mainArgsParam string
	if len(mainDecl.Params) > 1 {
		return "", fmt.Errorf("main: expected at most 1 parameter (args), got %d", len(mainDecl.Params))
	}
	if len(mainDecl.Params) == 1 {
		mainArgsParam = mainDecl.Params[0].Name
	}

	mainScope := newScope(nil)
	if mainArgsParam != "" {
		mainScope.define(mainArgsParam, TArray(TString()))
	}

	fb := &funcBuilder{cg: cg, fname: "main"}
	bodyC := fb.buildFunctionBody(mainScope, mainDecl.Body, "return 0;")
	if fb.retType.Kind != KVoid {
		return "", fmt.Errorf("main: must not return a value (got %s)", fb.retType.String())
	}

	var sb strings.Builder
	sb.WriteString(runtimePrelude)
	sb.WriteString("\n/* ---------------- generated program ---------------- */\n")

	for _, h := range cg.includeHeaders {
		sb.WriteString(fmt.Sprintf("#include <%s>\n", h))
	}
	sb.WriteString("\n")

	// Struct/closure/task typedefs and forward declarations, then bodies, are
	// appended progressively into cg.funcInstances / cg.classInstances as
	// codegen for main() (and everything it transitively calls) runs above.
	// We now flush everything in dependency-safe order: typedefs first (they
	// grow monotonically), then class structs, then function forward decls,
	// then function bodies, then main().

	sb.WriteString("/* global variables */\n")
	for _, name := range sortedKeys(cg.globalDecls) {
		g := cg.globalDecls[name]
		t, ok := cg.globalScope.lookup(g.Name)
		if !ok {
			continue // never referenced; skip (dead code)
		}
		sb.WriteString(fmt.Sprintf("static %s g_%s;\n", cg.ctype(t), g.Name))
	}
	sb.WriteString("\n")

	sb.WriteString("/* closures & tasks */\n")
	for _, td := range cg.typeDefs {
		sb.WriteString(td + "\n")
	}
	sb.WriteString("\n")

	sb.WriteString("/* classes */\n")
	for _, key := range cg.classOrder {
		ci := cg.classInstances[key]
		sb.WriteString(ci.StructC + "\n")
	}
	sb.WriteString("\n")

	sb.WriteString("/* forward declarations */\n")
	for _, key := range cg.funcOrder {
		fi := cg.funcInstances[key]
		sb.WriteString(fi.Forward + "\n")
	}
	for _, key := range cg.classOrder {
		ci := cg.classInstances[key]
		for _, mname := range sortedMethodKeys(ci.Methods) {
			sb.WriteString(ci.Methods[mname].Forward + "\n")
		}
	}
	sb.WriteString("\n")

	sb.WriteString("/* function bodies */\n")
	for _, key := range cg.funcOrder {
		fi := cg.funcInstances[key]
		sb.WriteString(fi.Body + "\n\n")
	}
	for _, key := range cg.classOrder {
		ci := cg.classInstances[key]
		for _, mname := range sortedMethodKeys(ci.Methods) {
			sb.WriteString(ci.Methods[mname].Body + "\n\n")
		}
	}

	sb.WriteString("static void nox_init_globals(void) {\n")
	for _, line := range cg.globalInitC {
		sb.WriteString("    " + line + "\n")
	}
	sb.WriteString("}\n\n")

	sb.WriteString("int main(int argc, char** argv) {\n")
	sb.WriteString("    nox_runtime_init(argc, argv);\n")
	sb.WriteString("    nox_init_globals();\n")
	sb.WriteString(indent(bodyC, "    "))
	sb.WriteString("}\n")

	return sb.String(), nil
}

func sortedKeys(m map[string]*ast.LetStmt) []string {
	var ks []string
	for k := range m {
		ks = append(ks, k)
	}
	sort.Strings(ks)
	return ks
}

func sortedMethodKeys(m map[string]*FuncInstance) []string {
	var ks []string
	for k := range m {
		ks = append(ks, k)
	}
	sort.Strings(ks)
	return ks
}

func indent(s, pre string) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	var sb strings.Builder
	for _, l := range lines {
		if l == "" {
			sb.WriteString("\n")
			continue
		}
		sb.WriteString(pre)
		sb.WriteString(l)
		sb.WriteString("\n")
	}
	return sb.String()
}

// prepassGlobals resolves and generates initializers for top-level `let`
// declarations, in file order (a global's initializer may only reference
// globals declared earlier in the same file).
func (cg *Codegen) prepassGlobals() {
	fb := &funcBuilder{cg: cg, fname: "__globals__"}
	for _, g := range cg.file.Globals {
		if g.Value == nil {
			if g.Type == nil {
				panic(fmt.Sprintf("nox: global 'let %s' needs an initializer or an explicit type", g.Name))
			}
			t := cg.resolveTypeExpr(g.Type)
			cg.globalScope.define(g.Name, t)
			continue
		}
		c, pre := newCtx(cg.globalScope)
		code, t := fb.genExpr(c, g.Value)
		if g.Type != nil {
			want := cg.resolveTypeExpr(g.Type)
			if !want.Equals(t) {
				panic(fmt.Sprintf("nox: global '%s': cannot assign %s to declared type %s", g.Name, t.String(), want.String()))
			}
			t = want
		}
		if t.ContainsUnknown() {
			panic(fmt.Sprintf("nox: global 'let %s': cannot infer the type from an empty array literal '[]'; add an explicit type (let %s: array<TYPE> = [])", g.Name, g.Name))
		}
		cg.globalScope.define(g.Name, t)
		for _, p := range *pre {
			cg.globalInitC = append(cg.globalInitC, strings.TrimRight(p, "\n"))
		}
		cg.globalInitC = append(cg.globalInitC, fmt.Sprintf("g_%s = %s;", g.Name, code))
	}
}
