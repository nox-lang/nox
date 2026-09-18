package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

func stdlibConstant(pkg, sym string) (string, Type, bool) {
	if pkg == "math" {
		switch sym {
		case "PI":
			return "3.14159265358979323846", TFloat(), true
		case "E":
			return "2.71828182845904523536", TFloat(), true
		}
	}
	return "", Type{}, false
}

func printCallFor(t Type, code string) string {
	switch t.Kind {
	case KInt:
		return fmt.Sprintf("nox_print_int(%s)", code)
	case KFloat:
		return fmt.Sprintf("nox_print_float(%s)", code)
	case KBool:
		return fmt.Sprintf("nox_print_bool(%s)", code)
	case KString:
		return fmt.Sprintf("nox_print_string(%s)", code)
	}
	panic(fmt.Sprintf("nox: cannot print a value of type %s", t.String()))
}

func (fb *funcBuilder) genStdlibCall(c *ctx, pkg, sym string, args []ast.Expr) (string, Type) {
	switch pkg {
	case "io":
		return fb.genIOCall(c, sym, args)
	case "random":
		return fb.genRandomCall(c, sym, args)
	case "fs":
		return fb.genFsCall(c, sym, args)
	case "path":
		return fb.genPathCall(c, sym, args)
	case "math":
		return fb.genMathCall(c, sym, args)
	case "time":
		return fb.genTimeCall(c, sym, args)
	}
	panic(fmt.Sprintf("nox: %s: unknown stdlib package '%s'", fb.fname, pkg))
}

// ---------------- io ----------------

func (fb *funcBuilder) genIOCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	switch sym {
	case "print", "println":
		for _, a := range args {
			code, t := fb.genExpr(c, a)
			c.emit(compilef("%s;", printCallFor(t, code)))
		}
		if sym == "println" {
			c.emit("nox_print_raw_cstr(\"\\n\");\n")
		}
		return "", TVoid()
	case "printf", "printfn":
		if len(args) == 0 {
			panic(fmt.Sprintf("nox: %s: io::%s requires a format string", fb.fname, sym))
		}
		fmtLit, ok := args[0].(*ast.StringLit)
		if !ok {
			panic(fmt.Sprintf("nox: %s: io::%s: the format string must be a string literal", fb.fname, sym))
		}
		fb.genFormatPrint(c, fmtLit.Value, args[1:])
		if sym == "printfn" {
			c.emit("nox_print_raw_cstr(\"\\n\");\n")
		}
		return "", TVoid()
	case "scan":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: io::scan() takes no arguments in this implementation; it returns the next whitespace-delimited token as a string", fb.fname))
		}
		return "nox_io_scan()", TString()
	case "scanln":
		if len(args) != 0 {
			panic(fmt.Sprintf("nox: %s: io::scanln() takes no arguments in this implementation; it returns the next line as a string", fb.fname))
		}
		return "nox_io_scanln()", TString()
	case "scanf":
		// Simplified: reads a full line, same as Scanln. Full scanf-style
		// format parsing is not implemented; use .toInt()/.toFloat() on the
		// returned string to convert individual values.
		return "nox_io_scanln()", TString()
	}
	panic(fmt.Sprintf("nox: %s: io has no function '%s'", fb.fname, sym))
}

// genFormatPrint lowers a Printf/Printfn-style `{}`-templated string,
// resolved entirely at compile time (the format string must be a literal).
func (fb *funcBuilder) genFormatPrint(c *ctx, format string, args []ast.Expr) {
	parts := strings.Split(format, "{}")
	if len(parts)-1 != len(args) {
		panic(fmt.Sprintf("nox: %s: format string has %d placeholder(s) but %d argument(s) were given", fb.fname, len(parts)-1, len(args)))
	}
	for i, part := range parts {
		if part != "" {
			c.emit(compilef("nox_print_raw_cstr(%s);", cStringLiteral(part)))
		}
		if i < len(args) {
			code, t := fb.genExpr(c, args[i])
			c.emit(compilef("%s;", printCallFor(t, code)))
		}
	}
}

// ---------------- random ----------------

func (fb *funcBuilder) genRandomCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	switch sym {
	case "rand":
		if len(args) == 0 {
			return "((int64_t)rand())", TInt()
		}
		if len(args) == 2 {
			a0, t0 := fb.genExpr(c, args[0])
			a1, t1 := fb.genExpr(c, args[1])
			if t0.Kind != KInt || t1.Kind != KInt {
				panic(fmt.Sprintf("nox: %s: random::rand(min, max) expects int arguments", fb.fname))
			}
			return fmt.Sprintf("((%s) + (int64_t)(rand() %% (((%s) - (%s)) + 1)))", a0, a1, a0), TInt()
		}
		panic(fmt.Sprintf("nox: %s: random::rand() takes zero or two arguments", fb.fname))
	case "randf":
		if len(args) == 0 {
			return "((double)rand() / (double)RAND_MAX)", TFloat()
		}
		if len(args) == 2 {
			a0 := fb.genFloatArg(c, args[0])
			a1 := fb.genFloatArg(c, args[1])
			return fmt.Sprintf("((%s) + ((double)rand() / (double)RAND_MAX) * ((%s) - (%s)))", a0, a1, a0), TFloat()
		}
		panic(fmt.Sprintf("nox: %s: random::randf() takes zero or two arguments", fb.fname))
	case "choice":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: random::choice(array) takes exactly one argument", fb.fname))
		}
		code, t := fb.genExpr(c, args[0])
		if t.Kind != KArray {
			panic(fmt.Sprintf("nox: %s: random::choice(array) expects an array", fb.fname))
		}
		elemType := *t.Elem
		elemC := fb.cg.ctype(elemType)
		arrTmp := fb.cg.freshTmp("choicearr")
		c.emit(compilef("nox_array %s = %s;", arrTmp, code))
		outTmp := fb.cg.freshTmp("choiceval")
		c.emit(compilef("%s %s;", elemC, outTmp))
		c.emit(compilef("nox_array_choice_raw(&%s, &%s, sizeof(%s));", arrTmp, outTmp, elemC))
		return outTmp, elemType
	case "shuffle":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: random::shuffle(array) takes exactly one argument", fb.fname))
		}
		lv, t := fb.genReceiverLvalue(c, args[0])
		if t.Kind != KArray {
			panic(fmt.Sprintf("nox: %s: random::shuffle(array) expects an array", fb.fname))
		}
		elemC := fb.cg.ctype(*t.Elem)
		c.emit(compilef("nox_array_shuffle_raw(&%s, sizeof(%s));", lv, elemC))
		return "", TVoid()
	}
	panic(fmt.Sprintf("nox: %s: random has no function '%s'", fb.fname, sym))
}

// ---------------- fs ----------------

func (fb *funcBuilder) genFsCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	strArg := func(i int) string {
		code, t := fb.genExpr(c, args[i])
		if t.Kind != KString {
			panic(fmt.Sprintf("nox: %s: fs::%s expects a string argument", fb.fname, sym))
		}
		return code
	}
	need := func(n int) {
		if len(args) != n {
			panic(fmt.Sprintf("nox: %s: fs::%s takes exactly %d argument(s)", fb.fname, sym, n))
		}
	}
	switch sym {
	case "read":
		need(1)
		return fmt.Sprintf("nox_fs_read(%s)", strArg(0)), TString()
	case "write":
		need(2)
		c.emit(compilef("nox_fs_write(%s, %s);", strArg(0), strArg(1)))
		return "", TVoid()
	case "append":
		need(2)
		c.emit(compilef("nox_fs_append(%s, %s);", strArg(0), strArg(1)))
		return "", TVoid()
	case "exists":
		need(1)
		return fmt.Sprintf("nox_fs_exists(%s)", strArg(0)), TBool()
	case "remove":
		need(1)
		c.emit(compilef("nox_fs_remove(%s);", strArg(0)))
		return "", TVoid()
	case "rename":
		need(2)
		c.emit(compilef("nox_fs_rename(%s, %s);", strArg(0), strArg(1)))
		return "", TVoid()
	case "copy":
		need(2)
		c.emit(compilef("nox_fs_copy(%s, %s);", strArg(0), strArg(1)))
		return "", TVoid()
	case "mkdir":
		need(1)
		c.emit(compilef("nox_fs_mkdir(%s);", strArg(0)))
		return "", TVoid()
	case "rmdir":
		need(1)
		c.emit(compilef("nox_fs_rmdir(%s);", strArg(0)))
		return "", TVoid()
	case "list":
		need(1)
		return fmt.Sprintf("nox_fs_list(%s)", strArg(0)), TArray(TString())
	}
	panic(fmt.Sprintf("nox: %s: fs has no function '%s'", fb.fname, sym))
}

