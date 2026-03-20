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

