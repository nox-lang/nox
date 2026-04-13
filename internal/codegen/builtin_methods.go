package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

var builtinMethodNames = map[string]bool{
	"push": true, "pop": true, "insert": true, "remove": true, "clear": true,
	"each": true, "eachIndex": true, "eachLine": true,
	"map": true, "filter": true, "find": true, "sort": true, "reverse": true,
	"contains": true, "startsWith": true, "endsWith": true, "substring": true, "empty": true,
	"toInt": true, "toFloat": true, "toBool": true, "toString": true,
}

