package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

// resolveCallArgs evaluates a call's argument expressions against a target
// parameter list, filling in default values and collecting a trailing
// variadic parameter into an array. It returns, for each *logical* parameter
// (variadic collapses to exactly one slot), the generated C code and Type.
