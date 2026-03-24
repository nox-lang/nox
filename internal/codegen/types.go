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
	KSlice  // []T   — growable, reference-like view (nox_slice)
	KArray  // [N]T  — fixed size, copied by value
	KMap    // map<K, V>
	KPointer
	KVoid
	KClass
	KFunc
	KTask   // Task<T>
	KThread // Thread
	KUnknown // element type of an empty slice literal, resolved from context
)

