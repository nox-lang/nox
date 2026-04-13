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

// mutatingBuiltinMethods are the built-ins that need a genuinely addressable
// receiver (see genReceiverLvalue) because they modify the receiver's
// underlying storage in place.
var mutatingBuiltinMethods = map[string]bool{
	"push": true, "pop": true, "insert": true, "remove": true, "clear": true, "sort": true,
}

// genReceiverLvalue evaluates e as the receiver of a built-in method call.
// If e naturally denotes storage (a variable, an index, or a class member),
// the *real* underlying storage is returned so that mutating methods
// (push/pop/insert/remove/clear/sort) affect it in place. Otherwise (e.g.
// the receiver is itself a call expression) the value is hoisted into a
// fresh local, which is still a valid receiver for read-only methods and is
// harmlessly "mutated in isolation" for the others.
func (fb *funcBuilder) genReceiverLvalue(c *ctx, e ast.Expr) (string, Type) {
	switch e.(type) {
	case *ast.Ident, *ast.IndexExpr, *ast.MemberExpr:
		return fb.genLvalue(c, e)
	}
	code, t := fb.genExpr(c, e)
	tmp := fb.cg.freshTmp("recv")
	c.emit(compilef("%s %s = %s;", fb.cg.ctype(t), tmp, code))
	return tmp, t
}

