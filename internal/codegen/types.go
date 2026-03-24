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

// Type is the codegen-level resolved type of a Nox value.
type Type struct {
	Kind Kind

	Elem *Type // element type for KSlice / KArray / KMap (value) / KPointer / KTask
	Key  *Type // key type for KMap
	Len  int64 // length for KArray

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
func TSlice(elem Type) Type {
	e := elem
	return Type{Kind: KSlice, Elem: &e}
}
func TArrayN(elem Type, n int64) Type {
	e := elem
	return Type{Kind: KArray, Elem: &e, Len: n}
}
func TMap(key, val Type) Type {
	k, v := key, val
	return Type{Kind: KMap, Key: &k, Elem: &v}
}
func TPointer(elem Type) Type {
	e := elem
	return Type{Kind: KPointer, Elem: &e}
}
func TTask(elem Type) Type {
	e := elem
	return Type{Kind: KTask, Elem: &e}
}
func TThread() Type { return Type{Kind: KThread} }

func (t Type) IsVoid() bool { return t.Kind == KVoid }

// ContainsUnknown reports whether t (or an element/key/pointee/task type
// nested within it) is the placeholder type produced by an empty literal
// whose element type could not be inferred from context.
func (t Type) ContainsUnknown() bool {
	switch t.Kind {
	case KUnknown:
		return true
	case KSlice, KArray, KPointer, KTask, KMap:
		if t.Key != nil && t.Key.ContainsUnknown() {
			return true
		}
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
	case KSlice, KPointer, KTask:
		if t.Elem == nil || o.Elem == nil {
			return t.Elem == o.Elem
		}
		return t.Elem.Equals(*o.Elem)
	case KArray:
		return t.Len == o.Len && t.Elem.Equals(*o.Elem)
	case KMap:
		return t.Key.Equals(*o.Key) && t.Elem.Equals(*o.Elem)
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
	case KSlice:
		return "[]" + t.Elem.String()
	case KArray:
		return fmt.Sprintf("[%d]%s", t.Len, t.Elem.String())
	case KMap:
		return "map<" + t.Key.String() + ", " + t.Elem.String() + ">"
	case KPointer:
		return "pointer<" + t.Elem.String() + ">"
	case KClass:
		return t.ClassName
	case KTask:
		return "Task<" + t.Elem.String() + ">"
	case KThread:
		return "Thread"
	case KFunc:
		var ps []string
		for _, p := range t.Params {
			ps = append(ps, p.String())
		}
		ret := ""
