package codegen

import (
	"fmt"
	"sort"
	"strings"

	"nox/internal/ast"
)

// freeVarNames returns the sorted set of identifiers referenced inside body
// that are not one of params and not locally bound somewhere in body (by
// `let`, a for-loop variable, or a try/catch variable). It is a
// syntax-level approximation (it does not model textual ordering/shadowing
