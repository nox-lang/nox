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

