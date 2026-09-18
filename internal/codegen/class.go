package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func findMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name {
			return m
		}
	}
	return nil
}

func fieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name {
			return f
		}
	}
	return nil
}

func fieldIsPrivate(decl *ast.ClassDecl, name string) bool {
	f := fieldDecl(decl, name)
	return f != nil && f.IsPrivate
}

func classCacheKey(className, argsKey string) string { return className + "#" + argsKey }

// genClassNew compiles `ClassName.new(args...)`: it instantiates
// (monomorphizes) the class for these constructor argument types the first
// time they're seen, and always emits a call to the resulting constructor.
func (fb *funcBuilder) genClassNew(c *ctx, className string, args []ast.Expr) (string, Type) {
	decl := fb.cg.classesByName[className]
	initDecl := findMethod(decl, "init")
	var initParams []*ast.Param
	if initDecl != nil {
		initParams = initDecl.Params
	} else if len(args) > 0 {
		panic(fmt.Sprintf("nox: %s: class '%s' has no 'init' but %s.new(...) was called with arguments", fb.fname, className, className))
	}
	argCodes, argTypes := fb.resolveCallArgs(c, className+".new", initParams, args, fb.cg.globalScope)

	key := classCacheKey(className, mangleList(argTypes))
	ci, ok := fb.cg.classCache[key]
	if !ok {
		ci = fb.cg.instantiateClass(className, decl, initDecl, argTypes)
		fb.cg.classCache[key] = ci
	}
	call := fmt.Sprintf("%s(%s)", ci.NewFuncName, strings.Join(argCodes, ", "))
	return call, Type{Kind: KClass, ClassName: ci.ClassName, ClassKey: ci.ClassKey}
}

