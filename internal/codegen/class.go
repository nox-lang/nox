package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func findMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name && !m.IsStatic {
			return m
		}
	}
	return nil
}

func findStaticMethod(decl *ast.ClassDecl, name string) *ast.FuncDecl {
	for _, m := range decl.Methods {
		if m.Name == name && m.IsStatic {
			return m
		}
	}
	return nil
}

func fieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name && !f.IsStatic {
			return f
		}
	}
	return nil
}

func staticFieldDecl(decl *ast.ClassDecl, name string) *ast.FieldDecl {
	for _, f := range decl.Fields {
		if f.Name == name && f.IsStatic {
			return f
		}
	}
	return nil
}

// A member (field or method) whose name does not start with an uppercase
// letter is only visible from inside its own class's own methods — the
// replacement for the removed `private` keyword (see resolve.go's
// isExported, which applies the same rule to package-level declarations).
func fieldIsPrivate(decl *ast.ClassDecl, name string) bool {
	f := fieldDecl(decl, name)
	return f != nil && !isExported(name)
}

func classCacheKey(className, argsKey string) string { return className + "#" + argsKey }

// ---------------- static class members ----------------
// A static field/method belongs to the class's NAME, not to any one
// monomorphized ClassInstance: `Counter.total` is one shared int no matter
// how many different field-type shapes Counter.new(...) has produced
// elsewhere in the program. Static fields are therefore registered once,
// eagerly, from every class declaration the compiler knows about (see
// registerStaticFields, called from NewCodegen and resolveImports) rather
// than lazily on first `.new()` the way instance fields are.

// registerStaticFields records the static fields of one class declaration.
// A static field must carry an explicit type (unlike an instance field, it
// has no constructor call to infer one from).
func (cg *Codegen) registerStaticFields(className string, decl *ast.ClassDecl) {
	for _, f := range decl.Fields {
		if !f.IsStatic {
			continue
		}
		key := className + "." + f.Name
		if _, already := cg.staticFieldType[key]; already {
			continue
		}
		if f.Type == nil {
			panic(fmt.Sprintf("nox: class '%s': static field '%s' needs an explicit type (a static field has no constructor call to infer one from)", className, f.Name))
		}
		cg.staticFieldType[key] = cg.resolveTypeExpr(f.Type)
		cg.staticFieldOrder = append(cg.staticFieldOrder, key)
		cg.staticFieldOwner[key] = decl
	}
}

func staticFieldCName(className, field string) string {
	return "cls_" + sanitizeIdent(className) + "_" + sanitizeIdent(field)
}

// prepassStaticFields resolves every static field's initializer (run once,
// after imports are resolved, alongside prepassGlobals) and emits its
// storage + nox_init_globals() assignment.
func (cg *Codegen) prepassStaticFields() {
	fb := &funcBuilder{cg: cg, fname: "__static_fields__"}
	for _, key := range cg.staticFieldOrder {
		parts := strings.SplitN(key, ".", 2)
		className, fieldName := parts[0], parts[1]
		decl := cg.staticFieldOwner[key]
		f := staticFieldDecl(decl, fieldName)
		t := cg.staticFieldType[key]
		cname := staticFieldCName(className, fieldName)
		if f.Default == nil {
			cg.globalInitC = append(cg.globalInitC, fmt.Sprintf("%s = %s;", cname, cg.zeroValueC(t)))
			continue
		}
		fb.currentClassName = className
		c, pre := newCtx(cg.globalScope)
		code, dt := fb.genExpr(c, f.Default)
		if !t.Equals(dt) {
			panic(fmt.Sprintf("nox: class '%s': static field '%s': default value is %s, expected %s", className, fieldName, dt.String(), t.String()))
		}
		for _, ln := range *pre {
			cg.globalInitC = append(cg.globalInitC, strings.TrimRight(ln, "\n"))
		}
		cg.globalInitC = append(cg.globalInitC, fmt.Sprintf("%s = %s;", cname, code))
	}
}

// genStaticFieldRead / genStaticFieldWrite implement `ClassName.field`.
func (fb *funcBuilder) genStaticFieldRead(className, fieldName string) (string, Type) {
	key := className + "." + fieldName
	t, ok := fb.cg.staticFieldType[key]
	if !ok {
		panic(fmt.Sprintf("nox: %s: class '%s' has no static field '%s'", fb.fname, className, fieldName))
	}
	decl := fb.cg.staticFieldOwner[key]
	if !isExported(fieldName) && fb.currentClassName != className {
		panic(fmt.Sprintf("nox: %s: '%s' is a private static field of class '%s'", fb.fname, fieldName, className))
	}
	_ = decl
	return staticFieldCName(className, fieldName), t
}

// genStaticCall compiles `ClassName.Method(args...)` (a static method call)
// or, if className is "Thread"/"Task", the corresponding built-in.
func (fb *funcBuilder) genStaticCall(c *ctx, className, methodName string, args []ast.Expr) (string, Type) {
	switch className {
	case "Thread":
		return fb.genThreadStatic(c, methodName, args)
	case "Task":
		return fb.genTaskStatic(c, methodName, args)
	}
	decl, ok := fb.cg.classesByName[className]
	if !ok {
		panic(fmt.Sprintf("nox: %s: unknown class '%s'", fb.fname, className))
	}
	mdecl := findStaticMethod(decl, methodName)
	if mdecl == nil {
		if findMethod(decl, methodName) != nil {
			panic(fmt.Sprintf("nox: %s: '%s' is an instance method of class '%s'; call it on an instance, not on the class itself", fb.fname, methodName, className))
		}
		panic(fmt.Sprintf("nox: %s: class '%s' has no static method '%s'", fb.fname, className, methodName))
	}
	if !isExported(methodName) && fb.currentClassName != className {
		panic(fmt.Sprintf("nox: %s: '%s' is a private static method of class '%s'", fb.fname, methodName, className))
	}
	argCodes, argTypes := fb.resolveCallArgs(c, className+"."+methodName, mdecl.Params, args, fb.cg.globalScope)
	name := "static:" + className + "::" + methodName
	fi := fb.cg.getOrInstantiateStaticFunc(name, className, mdecl, argTypes)
	call := fmt.Sprintf("%s(%s)", fi.MangledName, strings.Join(argCodes, ", "))
	if fi.IsAsync {
		return call, TTask(fi.RetType)
	}
	return call, fi.RetType
}

func (cg *Codegen) getOrInstantiateStaticFunc(name, className string, decl *ast.FuncDecl, argTypes []Type) *FuncInstance {
	key := funcKey{name: name, argsKey: mangleList(argTypes)}
	if fi, ok := cg.instCache[key]; ok {
		return fi
	}
	mangled := cg.freshName("nox_static_" + sanitizeIdent(className) + "_" + sanitizeIdent(decl.Name))
	fi := &FuncInstance{MangledName: mangled, Decl: decl, ParamTypes: argTypes, IsAsync: decl.IsAsync}
	if decl.ReturnType != nil {
		fi.RetType = cg.resolveTypeExpr(decl.ReturnType)
		fi.RetTypeKnown = true
	}
	fi.Emitting = true
	cg.instCache[key] = fi
	cg.funcInstances[mangled] = fi
	cg.funcOrder = append(cg.funcOrder, mangled)

	scope := newScope(nil)
	for i, p := range decl.Params {
		scope.define(p.Name, argTypes[i])
	}
	if decl.IsAsync {
		cg.emitAsyncFunc(fi, decl, argTypes, nil)
	} else {
		fb := &funcBuilder{cg: cg, fname: mangled, currentClassName: className, selfInstance: fi}
		if fi.RetTypeKnown {
			fb.retType = fi.RetType
			fb.retTypeKnown = true
		}
		bodyC := fb.buildFunctionBody(scope, decl.Body, "")
		fi.RetType = fb.retType
		fi.RetTypeKnown = true
		var cparams []string
		for i, p := range decl.Params {
			cparams = append(cparams, fmt.Sprintf("%s %s", cg.ctype(argTypes[i]), cIdent(p.Name)))
		}
		if len(cparams) == 0 {
			cparams = append(cparams, "void")
		}
		retC := "void"
		if fi.RetType.Kind != KVoid {
			retC = cg.ctype(fi.RetType)
		}
		fi.Forward = fmt.Sprintf("static %s %s(%s);", retC, mangled, strings.Join(cparams, ", "))
		fi.Body = fmt.Sprintf("static %s %s(%s) {\n%s}", retC, mangled, strings.Join(cparams, ", "), indent(bodyC, "    "))
	}
	fi.Emitting = false
	return fi
}

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

