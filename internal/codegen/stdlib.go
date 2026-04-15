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

