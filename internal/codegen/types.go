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

func TInt() Type    { return Type{Kind: KInt} }
func TFloat() Type  { return Type{Kind: KFloat} }
func TBool() Type   { return Type{Kind: KBool} }
func TString() Type { return Type{Kind: KString} }
func TVoid() Type   { return Type{Kind: KVoid} }
func TArray(elem Type) Type {
	e := elem
	return Type{Kind: KArray, Elem: &e}
}
func TPointer(elem Type) Type {
	e := elem
	return Type{Kind: KPointer, Elem: &e}
}
func TTask(elem Type) Type {
	e := elem
	return Type{Kind: KTask, Elem: &e}
}

func (t Type) IsVoid() bool { return t.Kind == KVoid }

// ContainsUnknown reports whether t (or an element/pointee/task type nested
// within it) is the placeholder type produced by an empty array literal
// `[]` whose element type could not be inferred from context.
func (t Type) ContainsUnknown() bool {
	switch t.Kind {
	case KUnknown:
		return true
	case KArray, KPointer, KTask:
		return t.Elem != nil && t.Elem.ContainsUnknown()
	}
	return false
}

func (t Type) Equals(o Type) bool {
	if t.Kind == KUnknown || o.Kind == KUnknown {
		return true
	}
	if t.Kind != o.Kind {
		return false
	}
	switch t.Kind {
	case KArray, KPointer, KTask:
		if t.Elem == nil || o.Elem == nil {
			return t.Elem == o.Elem
		}
		return t.Elem.Equals(*o.Elem)
	case KClass:
		return t.ClassKey == o.ClassKey
	case KFunc:
		if len(t.Params) != len(o.Params) {
			return false
		}
		for i := range t.Params {
			if !t.Params[i].Equals(o.Params[i]) {
				return false
			}
		}
		if (t.Ret == nil) != (o.Ret == nil) {
			return false
		}
		if t.Ret != nil && !t.Ret.Equals(*o.Ret) {
			return false
		}
		return true
	default:
		return true
	}
}

