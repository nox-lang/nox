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

