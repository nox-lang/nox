// Package codegen turns a Nox AST into a single C translation unit that is
// then compiled by tcc.
package codegen

import (
	"fmt"
	"strings"
)

type Kind int

const (
	KInt Kind = iota
	KFloat
	KBool
	KString
	KArray
	KPointer
	KVoid
	KClass
	KFunc
	KTask
	KUnknown // element type of an empty array literal, resolved from context
)

// Type is the codegen-level resolved type of a Nox value.
type Type struct {
	Kind Kind

	Elem *Type // element type for KArray / KPointer / KTask

	ClassName string // original Nox class name, for KClass
	ClassKey  string // mangled/instantiated struct tag, for KClass

	Params []Type // parameter types for KFunc
	Ret    *Type  // return type for KFunc (nil means void)
}

