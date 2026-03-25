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
		if t.Ret != nil {
			ret = ": " + t.Ret.String()
		}
		return "func(" + strings.Join(ps, ", ") + ")" + ret
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
	case KSlice:
		return "A" + mangle(*t.Elem)
	case KArray:
		return fmt.Sprintf("R%d_%s", t.Len, mangle(*t.Elem))
	case KMap:
		return "M" + mangle(*t.Key) + mangle(*t.Elem)
	case KPointer:
		return "P" + mangle(*t.Elem)
	case KTask:
		return "T" + mangle(*t.Elem)
	case KThread:
		return "H"
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

// isValidMapKey reports whether t may be used as a map key: types compared
// bytewise (int, bool, pointers, class references) or as strings.
func isValidMapKey(t Type) bool {
	switch t.Kind {
	case KInt, KBool, KString, KPointer, KClass:
		return true
	}
	return false
}

// mapKeyKindC returns the runtime key-kind constant for a key type.
func mapKeyKindC(t Type) string {
	if t.Kind == KString {
		return "NOX_KEY_STRING"
	}
	return "NOX_KEY_BYTES"
}

// ctype returns the C spelling for a Nox type, registering any auxiliary
// typedefs (closures, fixed arrays) it needs along the way.
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
	case KSlice:
		return "nox_slice"
	case KArray:
		return cg.ensureArrayType(t)
	case KMap:
		return "nox_map*"
	case KPointer:
		return cg.ctype(*t.Elem) + "*"
	case KClass:
		return "struct " + t.ClassKey + "*"
	case KFunc:
		return cg.ensureClosureType(t)
	case KTask:
		return "nox_task*"
	case KThread:
		return "nox_thread_obj*"
	}
	panic(fmt.Sprintf("ctype: unhandled kind %v", t.Kind))
}

// zeroValueC returns a C expression producing the zero/default value of t.
func (cg *Codegen) zeroValueC(t Type) string {
	switch t.Kind {
	case KInt:
		return "0"
	case KFloat:
		return "0.0"
	case KBool:
		return "false"
	case KString:
		return `nox_string_from_cstr("")`
	case KSlice:
		return "nox_slice_new()"
	case KArray:
		return "(" + cg.ctype(t) + "){0}"
	case KMap:
		return fmt.Sprintf("nox_map_new(%s, sizeof(%s), sizeof(%s))", mapKeyKindC(*t.Key), cg.ctype(*t.Key), cg.ctype(*t.Elem))
	case KPointer, KClass, KTask, KThread:
		return "NULL"
	case KVoid:
		return ""
	case KFunc:
		return "(" + cg.ctype(t) + "){0}"
	}
	return "0"
}

func (cg *Codegen) ensureClosureType(t Type) string {
	name := "NoxFn_" + mangle(t.Ret2()) + mangleList(t.Params)
	if _, ok := cg.closureTypes[name]; ok {
		return name
	}
	cg.closureTypes[name] = true
	retC := "void"
	if t.Ret != nil {
		retC = cg.ctype(*t.Ret)
	}
	var params []string
	params = append(params, "void*")
	for _, p := range t.Params {
		params = append(params, cg.ctype(p))
	}
	cg.typeDefs = append(cg.typeDefs, fmt.Sprintf("typedef struct { %s (*fn)(%s); void* env; } %s;",
		retC, strings.Join(params, ", "), name))
	return name
}

// Ret2 normalizes a nil Ret into TVoid for mangling purposes.
func (t Type) Ret2() Type {
	if t.Ret == nil {
		return TVoid()
