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

// ---------------- path ----------------

func (fb *funcBuilder) genPathCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	strArg := func(i int) string {
		code, t := fb.genExpr(c, args[i])
		if t.Kind != KString {
			panic(fmt.Sprintf("nox: %s: path::%s expects a string argument", fb.fname, sym))
		}
		return code
	}
	switch sym {
	case "join":
		if len(args) < 2 {
			panic(fmt.Sprintf("nox: %s: path::join(...) takes at least two arguments", fb.fname))
		}
		acc := strArg(0)
		for i := 1; i < len(args); i++ {
			acc = fmt.Sprintf("nox_path_join2(%s, %s)", acc, strArg(i))
		}
		return acc, TString()
	case "basename":
		return fmt.Sprintf("nox_path_basename(%s)", strArg(0)), TString()
	case "dirname":
		return fmt.Sprintf("nox_path_dirname(%s)", strArg(0)), TString()
	case "ext":
		return fmt.Sprintf("nox_path_ext(%s)", strArg(0)), TString()
	case "stem":
		return fmt.Sprintf("nox_path_stem(%s)", strArg(0)), TString()
	case "absolute":
		return fmt.Sprintf("nox_path_absolute(%s)", strArg(0)), TString()
	}
	panic(fmt.Sprintf("nox: %s: path has no function '%s'", fb.fname, sym))
}

// ---------------- math ----------------

// genFloatArg accepts either an int or a float argument, widening int to
// float automatically (stdlib convenience; the core language itself
// performs no implicit conversions — see requireSameNumeric).
func (fb *funcBuilder) genFloatArg(c *ctx, e ast.Expr) string {
	code, t := fb.genExpr(c, e)
	switch t.Kind {
	case KFloat:
		return code
	case KInt:
		return fmt.Sprintf("((double)(%s))", code)
	}
	panic(fmt.Sprintf("nox: %s: expected a numeric argument, got %s", fb.fname, t.String()))
}

func (fb *funcBuilder) genMathCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	unaryFloat := func(cfn string) (string, Type) {
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: math::%s(x) takes exactly one argument", fb.fname, sym))
		}
		return fmt.Sprintf("%s(%s)", cfn, fb.genFloatArg(c, args[0])), TFloat()
	}
	switch sym {
	case "abs":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: math::abs(x) takes exactly one argument", fb.fname))
		}
		code, t := fb.genExpr(c, args[0])
		switch t.Kind {
		case KInt:
			return fmt.Sprintf("nox_math_abs_i(%s)", code), TInt()
		case KFloat:
			return fmt.Sprintf("nox_math_abs_f(%s)", code), TFloat()
		}
		panic(fmt.Sprintf("nox: %s: math::abs(x) expects an int or float", fb.fname))
	case "min", "max":
		if len(args) != 2 {
			panic(fmt.Sprintf("nox: %s: math::%s(a, b) takes exactly two arguments", fb.fname, sym))
		}
		a0, t0 := fb.genExpr(c, args[0])
		a1, t1 := fb.genExpr(c, args[1])
		if !t0.Equals(t1) || (t0.Kind != KInt && t0.Kind != KFloat) {
			panic(fmt.Sprintf("nox: %s: math::%s(a, b) expects two arguments of the same numeric type", fb.fname, sym))
		}
		suffix := "_i"
		if t0.Kind == KFloat {
			suffix = "_f"
		}
		return fmt.Sprintf("nox_math_%s%s(%s, %s)", sym, suffix, a0, a1), t0
	case "pow":
		if len(args) != 2 {
			panic(fmt.Sprintf("nox: %s: math::pow(x, y) takes exactly two arguments", fb.fname))
		}
		return fmt.Sprintf("pow(%s, %s)", fb.genFloatArg(c, args[0]), fb.genFloatArg(c, args[1])), TFloat()
	case "sqrt":
		return unaryFloat("sqrt")
	case "floor":
		return unaryFloat("floor")
	case "ceil":
		return unaryFloat("ceil")
	case "round":
		return unaryFloat("round")
	case "sin":
		return unaryFloat("sin")
	case "cos":
		return unaryFloat("cos")
	case "tan":
		return unaryFloat("tan")
	case "asin":
		return unaryFloat("asin")
	case "acos":
		return unaryFloat("acos")
	case "atan":
		return unaryFloat("atan")
	case "log":
		return unaryFloat("log")
	case "log10":
		return unaryFloat("log10")
	case "exp":
		return unaryFloat("exp")
	}
	panic(fmt.Sprintf("nox: %s: math has no function '%s'", fb.fname, sym))
}

// ---------------- time ----------------

func (fb *funcBuilder) genTimeCall(c *ctx, sym string, args []ast.Expr) (string, Type) {
	intArg := func(i int) string {
		code, t := fb.genExpr(c, args[i])
		if t.Kind != KInt {
			panic(fmt.Sprintf("nox: %s: time::%s expects an int argument", fb.fname, sym))
		}
		return code
	}
	switch sym {
	case "now":
		return "nox_time_now()", TInt()
	case "unix":
		return "nox_time_unix()", TInt()
	case "sleep":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: time::sleep(seconds) takes exactly one argument", fb.fname))
		}
		c.emit(compilef("nox_time_sleep(%s);", fb.genFloatArg(c, args[0])))
		return "", TVoid()
	case "clock":
		return "nox_time_clock()", TFloat()
	case "year", "month", "day", "hour", "minute", "second":
		if len(args) != 1 {
			panic(fmt.Sprintf("nox: %s: time::%s(t) takes exactly one argument", fb.fname, sym))
		}
		return fmt.Sprintf("nox_time_%s(%s)", sym, intArg(0)), TInt()
	}
	panic(fmt.Sprintf("nox: %s: time has no function '%s'", fb.fname, sym))
}

// ---------------- raw C (`include`d headers) ----------------

