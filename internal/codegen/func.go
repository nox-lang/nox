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

type deferEntry struct {
	flagVar string
	bodyC   string
}

type loopCtx struct {
	mode          string // "collect" | "breakvalue" | "hofvalue" | "plain"
	isSwitch      bool   // true if this context represents a `switch`, not a loop
	collectVar    string
	elemType      *Type
	resultVar     string
	brokeVar      string
	resultType    *Type
	hofLabel      string // for "hofvalue": label to jump to after storing the result
	continueLabel string // for real loops: label `next`/`next value` jumps to
}

// ctx threads a per-statement hoisting buffer through expression codegen.
type ctx struct {
	scope *Scope
	pre   *[]string
}

