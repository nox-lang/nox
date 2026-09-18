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

func (*AssignStmt) stmtNode() {}

type IfStmt struct {
	Base
	Cond Expr
	Then *BlockStmt
	Else Stmt // *IfStmt, *BlockStmt, or nil
}

func (*IfStmt) stmtNode() {}
func (*IfStmt) exprNode() {}

// ForCondStmt is `for (condition) { ... }`. Cond may be nil for an infinite loop.
type ForCondStmt struct {
	Base
	Cond Expr
	Body *BlockStmt
}

func (*ForCondStmt) stmtNode() {}
func (*ForCondStmt) exprNode() {}

// ForInStmt is `for (value in array) {}` or `for (index, value in array) {}`.
type ForInStmt struct {
	Base
	IndexName string // "" if not requested
	ValueName string
	Array     Expr
	Body      *BlockStmt
}

func (*ForInStmt) stmtNode() {}
func (*ForInStmt) exprNode() {}

type WhileStmt struct {
	Base
	Cond Expr
	Body *BlockStmt
}

func (*WhileStmt) stmtNode() {}
func (*WhileStmt) exprNode() {}

type BreakStmt struct {
	Base
	Value Expr // nil if bare `break`
}

func (*BreakStmt) stmtNode() {}

// NextStmt is a loop's `next` / `next value` (like C's `continue`, and able
// to carry a value into the loop's collected result — see its use in
// codegen). Value is nil for a bare `next`.
type NextStmt struct {
	Base
	Value Expr
}

func (*NextStmt) stmtNode() {}

// YieldStmt produces a value from within a `.each`/`.map`/`.filter`/`.find`
// callback (or a `.sort` comparator) for the current element; unlike
// `return`, it does not exit the enclosing function. Value is required.
type YieldStmt struct {
	Base
	Value Expr
}

func (*YieldStmt) stmtNode() {}

type ReturnStmt struct {
	Base
	Value Expr // nil if bare `return`
}

func (*ReturnStmt) stmtNode() {}

type SwitchCase struct {
	Base
	Values []Expr // empty for default
	Body   *BlockStmt
}

type SwitchStmt struct {
	Base
	Subject Expr
	Cases   []*SwitchCase
	Default *BlockStmt // nil if no default
}

func (*SwitchStmt) stmtNode() {}
func (*SwitchStmt) exprNode() {}

type DeferStmt struct {
	Base
	Body *BlockStmt
}

func (*DeferStmt) stmtNode() {}

type TryStmt struct {
	Base
	Body      *BlockStmt
	CatchVar  string
	CatchBody *BlockStmt
}

func (*TryStmt) stmtNode() {}

// ---------- Expressions ----------

type Ident struct {
	Base
	Name string
}

func (*Ident) exprNode() {}

// QualIdent is a `::`-separated path such as `math::sqrt` or `libs::math::add`.
type QualIdent struct {
	Base
	Parts []string
}

func (*QualIdent) exprNode() {}

type ThisExpr struct{ Base }

func (*ThisExpr) exprNode() {}

type IntLit struct {
	Base
	Value int64
}

func (*IntLit) exprNode() {}

type FloatLit struct {
	Base
	Value float64
}

func (*FloatLit) exprNode() {}

type StringLit struct {
	Base
	Value string
}

func (*StringLit) exprNode() {}

type BoolLit struct {
	Base
	Value bool
}

func (*BoolLit) exprNode() {}

type NullLit struct{ Base }

