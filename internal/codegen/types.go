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

func (t Type) String() string {
	switch t.Kind {
	case KInt:
		return "int"
	case KFloat:
		return "float"
	case KBool:
		return "bool"
	case KString:
		return "string"
	case KVoid:
		return "void"
	case KArray:
		return "array<" + t.Elem.String() + ">"
	case KPointer:
		return "pointer<" + t.Elem.String() + ">"
	case KClass:
		return t.ClassName
	case KTask:
		return "Task<" + t.Elem.String() + ">"
	case KFunc:
		var ps []string
		for _, p := range t.Params {
			ps = append(ps, p.String())
		}
		ret := "void"
		if t.Ret != nil {
			ret = t.Ret.String()
		}
		return "func(" + strings.Join(ps, ", ") + "):" + ret
	}
	return "?"
}

// mangle produces a short, unique, C-identifier-safe fragment identifying a
// type, used to build monomorphized function/class/closure names.
func mangle(t Type) string {
	switch t.Kind {
	case KInt:
		return "i"
	case KFloat:
		return "f"
	case KBool:
		return "b"
	case KString:
		return "s"
	case KVoid:
		return "v"
	case KArray:
		return "A" + mangle(*t.Elem)
	case KPointer:
		return "P" + mangle(*t.Elem)
	case KTask:
		return "T" + mangle(*t.Elem)
	case KClass:
		return "C" + t.ClassKey
	case KFunc:
		var sb strings.Builder
		sb.WriteString("F")
		for _, p := range t.Params {
			sb.WriteString(mangle(p))
		}
		sb.WriteString("_")
		if t.Ret != nil {
			sb.WriteString(mangle(*t.Ret))
		} else {
			sb.WriteString("v")
		}
		return sb.String()
	}
	return "x"
}

func mangleList(ts []Type) string {
	var sb strings.Builder
	for _, t := range ts {
		sb.WriteString("_")
		sb.WriteString(mangle(t))
	}
	return sb.String()
}

// ctype returns the C spelling for a Nox type, registering any auxiliary
// typedefs (closures, tasks) it needs along the way.
func (cg *Codegen) ctype(t Type) string {
	switch t.Kind {
	case KInt:
		return "int64_t"
	case KFloat:
		return "double"
	case KBool:
		return "bool"
	case KString:
		return "nox_string"
	case KVoid:
		return "void"
	case KArray:
		return "nox_array"
	case KPointer:
		return cg.ctype(*t.Elem) + "*"
	case KClass:
		return "struct " + t.ClassKey + "*"
	case KFunc:
		return cg.ensureClosureType(t)
	case KTask:
		return cg.ensureTaskType(t)
	}
	panic(fmt.Sprintf("ctype: unhandled kind %v", t.Kind))
}

