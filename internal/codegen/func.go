package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
	"nox/internal/token"
)

// funcBuilder holds the mutable state needed while generating the C code for
// one function/method body (or the top-level main/global-init pseudo body).
type funcBuilder struct {
	cg    *Codegen
	fname string

	currentClassKey string        // set while generating a class method body, for private-access checks
	selfInstance    *FuncInstance // back-reference so a self-recursive call can see the return type as soon as it's known, not just after the whole body finishes

	retTypeKnown bool
	retType      Type
	isAsync      bool

	defers    []*deferEntry
	loopStack []*loopCtx
	tryStack  []*tryCtx

	deferFlagQueue []string
	deferFlagIdx   int
}

type tryCtx struct {
	catchLabel string
}

