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

// instantiateClass creates a new monomorphized struct + constructor for a
// class given concrete constructor argument types.
func (cg *Codegen) instantiateClass(className string, decl *ast.ClassDecl, initDecl *ast.FuncDecl, argTypes []Type) *ClassInstance {
	classKey := cg.freshName("Nox_" + sanitizeIdent(className))
	ci := &ClassInstance{
		ClassKey:   classKey,
		ClassName:  className,
		Decl:       decl,
		FieldTypes: map[string]Type{},
		Methods:    map[string]*FuncInstance{},
	}
	cg.classInstances[classKey] = ci
	cg.classOrder = append(cg.classOrder, classKey)

	thisType := Type{Kind: KClass, ClassName: className, ClassKey: classKey}

	// Pre-seed field types from explicit annotations so `init` can rely on
	// them (and so classes with no `init` at all still work).
	for _, f := range decl.Fields {
		if f.Type != nil {
			t := cg.resolveTypeExpr(f.Type)
			ci.FieldTypes[f.Name] = t
			ci.FieldOrder = append(ci.FieldOrder, f.Name)
		}
	}

	var initFuncName string
	if initDecl != nil {
		scope := newScope(nil)
		scope.define("this", thisType)
		for i, p := range initDecl.Params {
			scope.define(p.Name, argTypes[i])
		}
		ifb := &funcBuilder{cg: cg, fname: classKey + "_init", currentClassKey: classKey}
		initBodyC := ifb.buildFunctionBody(scope, initDecl.Body, "")
		initFuncName = classKey + "_init"
		var initParams []string
		initParams = append(initParams, fmt.Sprintf("struct %s* %s", classKey, cIdent("this")))
		for i, p := range initDecl.Params {
			initParams = append(initParams, fmt.Sprintf("%s %s", cg.ctype(argTypes[i]), cIdent(p.Name)))
		}
		initFI := &FuncInstance{
			MangledName: initFuncName,
			Forward:     fmt.Sprintf("static void %s(%s);", initFuncName, strings.Join(initParams, ", ")),
			Body:        fmt.Sprintf("static void %s(%s) {\n%s}", initFuncName, strings.Join(initParams, ", "), indent(initBodyC, "    ")),
		}
		cg.funcInstances[initFuncName] = initFI
		cg.funcOrder = append(cg.funcOrder, initFuncName)
	}

	// Any field not yet typed (not annotated, and not assigned in `init`)
	// falls back to its default expression's type, if it has one.
	for _, f := range decl.Fields {
		if _, ok := ci.FieldTypes[f.Name]; ok {
			continue
		}
		if f.Default == nil {
			panic(fmt.Sprintf("nox: class '%s': cannot infer the type of field '%s' (it is never assigned in 'init' and has no default or explicit type)", className, f.Name))
		}
		dfb := &funcBuilder{cg: cg, fname: classKey + "_field_default"}
		dc, _ := newCtx(cg.globalScope)
		_, t := dfb.genExpr(dc, f.Default)
		ci.FieldTypes[f.Name] = t
		ci.FieldOrder = append(ci.FieldOrder, f.Name)
	}

	// Struct definition.
	var fieldsText strings.Builder
	for _, name := range ci.FieldOrder {
		fieldsText.WriteString(fmt.Sprintf("    %s %s;\n", cg.ctype(ci.FieldTypes[name]), name))
	}
	ci.StructC = fmt.Sprintf("struct %s {\n%s};", classKey, fieldsText.String())
	ci.StructEmitted = true

	// Constructor: allocate, apply field defaults, call `init` (a genuinely
	// separate function — see above; inlining its body directly here would
	// make init's own implicit `return;` end the constructor early, before
	// it has a chance to return the new instance), then return the instance.
	var ctorBody strings.Builder
	ctorBody.WriteString(fmt.Sprintf("struct %s* %s = (struct %s*)GC_MALLOC(sizeof(struct %s));\n", classKey, cIdent("this"), classKey, classKey))
	for _, f := range decl.Fields {
		if f.Default == nil {
			continue
		}
		dfb := &funcBuilder{cg: cg, fname: classKey + "_new"}
		dc, dpre := newCtx(newScope(nil))
		code, _ := dfb.genExpr(dc, f.Default)
		for _, ln := range *dpre {
			ctorBody.WriteString(ln)
		}
		ctorBody.WriteString(fmt.Sprintf("%s->%s = %s;\n", cIdent("this"), f.Name, code))
	}
	var ctorParams []string
	if initDecl != nil {
		var callArgs []string
		callArgs = append(callArgs, cIdent("this"))
		for i, p := range initDecl.Params {
			ctorParams = append(ctorParams, fmt.Sprintf("%s %s", cg.ctype(argTypes[i]), cIdent(p.Name)))
			callArgs = append(callArgs, cIdent(p.Name))
		}
		ctorBody.WriteString(fmt.Sprintf("%s(%s);\n", initFuncName, strings.Join(callArgs, ", ")))
	}
	ctorBody.WriteString(fmt.Sprintf("return %s;\n", cIdent("this")))
	if len(ctorParams) == 0 {
		ctorParams = append(ctorParams, "void")
	}
	ci.NewFuncName = classKey + "_new"
	retC := fmt.Sprintf("struct %s*", classKey)
	newFI := &FuncInstance{
		MangledName: ci.NewFuncName,
		Forward:     fmt.Sprintf("static %s %s(%s);", retC, ci.NewFuncName, strings.Join(ctorParams, ", ")),
		Body:        fmt.Sprintf("static %s %s(%s) {\n%s}", retC, ci.NewFuncName, strings.Join(ctorParams, ", "), indent(ctorBody.String(), "    ")),
	}
	cg.funcInstances[newFI.MangledName] = newFI
	cg.funcOrder = append(cg.funcOrder, newFI.MangledName)

	return ci
}

// genMethodCall compiles `recv.methodName(args...)` where recv is a class
// value, monomorphizing the method for these argument types on first use.
func (fb *funcBuilder) genMethodCall(c *ctx, recvCode string, recvType Type, methodName string, args []ast.Expr) (string, Type) {
	ci := fb.cg.classInstances[recvType.ClassKey]
	if methodName == "init" {
		panic(fmt.Sprintf("nox: %s: 'init' cannot be called directly; use %s.new(...)", fb.fname, ci.ClassName))
	}
	mdecl := findMethod(ci.Decl, methodName)
	if mdecl == nil {
		panic(fmt.Sprintf("nox: %s: class '%s' has no method '%s'", fb.fname, ci.ClassName, methodName))
	}
	if mdecl.IsPrivate && fb.currentClassKey != recvType.ClassKey {
		panic(fmt.Sprintf("nox: %s: '%s' is a private method of class '%s'", fb.fname, methodName, ci.ClassName))
	}

	recvTmp := fb.cg.freshTmp("recv")
	c.emit(compilef("%s %s = %s;", fb.cg.ctype(recvType), recvTmp, recvCode))
	argCodes, argTypes := fb.resolveCallArgs(c, ci.ClassName+"."+methodName, mdecl.Params, args, fb.cg.globalScope)

	mkey := methodName + "#" + mangleList(argTypes)
	fi, ok := ci.Methods[mkey]
	if !ok {
		mangled := fb.cg.freshName(ci.ClassKey + "_" + sanitizeIdent(methodName))
		fi = &FuncInstance{MangledName: mangled, Decl: mdecl, ParamTypes: argTypes, IsAsync: mdecl.IsAsync}
		if mdecl.ReturnType != nil {
			fi.RetType = fb.cg.resolveTypeExpr(mdecl.ReturnType)
			fi.RetTypeKnown = true
		}
		ci.Methods[mkey] = fi // register before generating, for recursive self-calls
		thisType := Type{Kind: KClass, ClassName: ci.ClassName, ClassKey: ci.ClassKey}
		if mdecl.IsAsync {
			fb.cg.emitAsyncFunc(fi, mdecl, argTypes, &thisType)
		} else {
			fb.cg.emitSyncFuncWithClass(fi, mdecl, argTypes, thisType, ci.ClassKey)
		}
	}
	callArgs := append([]string{recvTmp}, argCodes...)
	call := fmt.Sprintf("%s(%s)", fi.MangledName, strings.Join(callArgs, ", "))
	if fi.IsAsync {
		return call, TTask(fi.RetType)
	}
	return call, fi.RetType
}

