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

// ---------- Declarations ----------

type Param struct {
	Base
	Name     string
	Type     *TypeExpr // nil if inferred
	Default  Expr      // nil if none
	Variadic bool
}

type FuncDecl struct {
	Base
	Name       string
	Params     []*Param
	ReturnType *TypeExpr // nil if inferred
	Body       *BlockStmt
	IsPrivate  bool
	IsAsync    bool
}

type FieldDecl struct {
	Base
	Name      string
	Type      *TypeExpr // nil if inferred from constructor usage
	Default   Expr      // nil if none
	IsPrivate bool
}

type ClassDecl struct {
	Base
	Name      string
	Fields    []*FieldDecl
	Methods   []*FuncDecl
	IsPrivate bool
}

// ---------- Statements ----------

type BlockStmt struct {
	Base
	Stmts []Stmt
}

func (*BlockStmt) stmtNode() {}

type LetStmt struct {
	Base
	Name      string
	Type      *TypeExpr // nil if inferred
	Value     Expr      // nil if uninitialized (`let value`)
	IsPrivate bool      // only meaningful at package (top) level
}

func (*LetStmt) stmtNode() {}

type ExprStmt struct {
	Base
	X Expr
}

func (*ExprStmt) stmtNode() {}

type AssignStmt struct {
	Base
	Target Expr
	Op     token.Kind // ASSIGN, PLUSEQ, MINUSEQ, STAREQ, SLASHEQ
	Value  Expr
}

