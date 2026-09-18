// Package ast defines the abstract syntax tree produced by the Nox parser.
package ast

import "nox/internal/token"

type Node interface{ Pos() (int, int) }

type Expr interface {
	Node
	exprNode()
}

type Stmt interface {
	Node
	stmtNode()
}

type Base struct{ Line, Col int }

func (b Base) Pos() (int, int) { return b.Line, b.Col }

// ---------- Type expressions ----------

// TypeExpr is a parsed (unresolved) type annotation, e.g. `int`, `array<string>`,
// `pointer<int>`, or a class name.
type TypeExpr struct {
	Base
	Name string    // "int", "float", "bool", "string", "array", "pointer", or a class/user name
	Elem *TypeExpr // element type for array<T> / pointer<T>
}

// ---------- File ----------

type ImportSpec struct {
	Base
	Path  string
	Alias string // "" if none given (defaults to path)
}

type IncludeSpec struct {
	Base
	Header string
	Alias  string // "" if none given (defaults to header stem)
}

type File struct {
	Base
	Package  string
	Imports  []*ImportSpec
	Includes []*IncludeSpec
	Funcs    []*FuncDecl
	Classes  []*ClassDecl
	Globals  []*LetStmt
	Filename string
}

