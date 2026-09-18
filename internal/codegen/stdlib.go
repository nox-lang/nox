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

