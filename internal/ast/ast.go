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

// TypeExpr is a parsed (unresolved) type annotation, e.g. `int`, `[]string`
// (slice), `[3]int` (array), `map<string, int>`, `pointer<int>`,
// `Task<int>`, `func(int): int`, or a class / alias name.
type TypeExpr struct {
	Base
	// Name is one of: "int", "float", "bool", "string", "slice", "array",
	// "map", "pointer", "Task", "Thread", "func", or a class / type-alias name.
	Name   string
	Elem   *TypeExpr   // element type: slice / array / map value / pointer / Task
	Key    *TypeExpr   // key type for map<K, V>
	Len    int64       // length for a fixed-size array
	Params []*TypeExpr // parameter types for func(...)
	Ret    *TypeExpr   // result type for func(...): R (nil = no result)
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

// TypeDecl is `type Name = T` / `type Name T` (a transparent type alias).
type TypeDecl struct {
	Base
	Name string
	Type *TypeExpr
}

type File struct {
	Base
	Package  string
	Imports  []*ImportSpec
	Includes []*IncludeSpec
	Funcs    []*FuncDecl
	Classes  []*ClassDecl
	Globals  []*LetStmt
	Types    []*TypeDecl
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
	IsStatic   bool // `static func` inside a class
	IsAsync    bool
}

type FieldDecl struct {
	Base
	Name      string
	Type      *TypeExpr // nil if inferred from constructor usage
	Default   Expr      // nil if none
	IsStatic  bool      // `static let` inside a class (a class variable)
}

type ClassDecl struct {
	Base
	Name    string
	Fields  []*FieldDecl
	Methods []*FuncDecl
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

