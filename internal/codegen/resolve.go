package codegen

import (
	"fmt"

	"nox/internal/ast"
)

// resolveTypeExpr turns a parsed type annotation into a concrete codegen
// Type. Built-in kinds (int/float/bool/string/array<T>/pointer<T>) always
// resolve. A bare class name resolves only if that class has already been
// instantiated exactly once somewhere in the program (Nox classes are
// monomorphized like functions; see class.go), which covers the common case
// of explicit class-typed parameters/locals following at least one
// `ClassName.new(...)` call earlier in the program. This is a deliberate,
// documented simplification of full generic class annotations.
func (cg *Codegen) resolveTypeExpr(te *ast.TypeExpr) Type {
	switch te.Name {
	case "int":
		return TInt()
	case "float":
		return TFloat()
	case "bool":
		return TBool()
	case "string":
		return TString()
	case "array":
		if te.Elem == nil {
			panic("nox: 'array' type requires an element type, e.g. array<int>")
		}
		return TArray(cg.resolveTypeExpr(te.Elem))
	case "pointer":
		if te.Elem == nil {
			panic("nox: 'pointer' type requires a pointee type, e.g. pointer<int>")
		}
		return TPointer(cg.resolveTypeExpr(te.Elem))
	}
	if _, ok := cg.classesByName[te.Name]; ok {
		for _, key := range cg.classOrder {
			ci := cg.classInstances[key]
			if ci.ClassName == te.Name {
				return Type{Kind: KClass, ClassName: ci.ClassName, ClassKey: ci.ClassKey}
			}
		}
		panic(fmt.Sprintf("nox: type annotation '%s' refers to a class with no instantiation yet; call %s.new(...) at least once before using '%s' as an explicit type", te.Name, te.Name, te.Name))
	}
	panic(fmt.Sprintf("nox: unknown type '%s'", te.Name))
}

