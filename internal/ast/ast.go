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
