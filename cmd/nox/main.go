// Command nox is the command-line interface for the Nox toolchain:
// `nox init`, `nox build`, and `nox get`, as described in the language
// spec's package-management section.
package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"nox/internal/ast"
	"nox/internal/codegen"
	"nox/internal/parser"
	"nox/internal/pkgmgr"
	"nox/internal/toolchain"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}
	var err error
	switch os.Args[1] {
	case "init":
		err = cmdInit(os.Args[2:])
	case "build":
		err = cmdBuild(os.Args[2:])
	case "get":
