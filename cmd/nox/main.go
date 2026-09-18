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
	noxruntime "nox/internal/runtime"
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
		err = cmdGet(os.Args[2:])
	case "version", "-v", "--version":
		fmt.Println("nox version 0.1.0")
		return
	case "help", "-h", "--help":
		printUsage()
		return
	default:
		printUsage()
		os.Exit(1)
	}
	if err != nil {
		fmt.Fprintf(os.Stderr, "nox: %s\n", err)
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Println(`nox — the Nox language toolchain

Usage:
  nox init <name>          Scaffold a new Nox package in ./<name>
  nox build                Build the package in the current directory (nox.toml + src/) -> build/<name>
  nox build <file.nox>     Build a single file -> an executable next to it (no build/ directory)
  nox build <file.nox> --emit-c   Same, and also keep the generated <file>.c next to it
  nox get <source>         Fetch a dependency (e.g. github.com/user/repo) via git

Environment variables (for 'nox build'):
  NOX_OS=linux|windows      Target OS (default: this machine's OS)
  NOX_ARCH=amd64|arm64      Target architecture (default: this machine's arch)
  NOX_TCC=/path/to/tcc      Which tcc binary to invoke (default: "tcc" on PATH).
                            Point this at a tcc build for another target to
                            cross-compile (e.g. a Windows-target tcc).`)
}

// ---------------- init ----------------

func cmdInit(args []string) error {
	if len(args) != 1 {
		return fmt.Errorf("usage: nox init <name>")
	}
	name := args[0]
	if err := pkgmgr.Init(name, name); err != nil {
		return err
	}
	fmt.Printf("Created Nox package '%s' in ./%s\n", name, name)
	return nil
}

// ---------------- get ----------------

func cmdGet(args []string) error {
	if len(args) != 1 {
		return fmt.Errorf("usage: nox get <source>  (e.g. nox get github.com/user/repo)")
	}
	_, root, ok := pkgmgr.FindManifest(".")
	if !ok {
		return fmt.Errorf("no nox.toml found (run this inside a package created with 'nox init')")
	}
	manifestPath := filepath.Join(root, "nox.toml")
	m, err := pkgmgr.Load(manifestPath)
	if err != nil {
		return err
	}
	dest, err := pkgmgr.Get(root, args[0])
	if err != nil {
		return err
	}
	name := filepath.Base(args[0])
	m.Dependencies[name] = args[0]
	if err := m.Save(manifestPath); err != nil {
		return err
	}
	fmt.Printf("Fetched %s -> %s\n", args[0], dest)
	return nil
}

// ---------------- build ----------------

