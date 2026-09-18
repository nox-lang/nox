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

func cmdBuild(args []string) error {
	emitC := false
	var file string
	for _, a := range args {
		switch {
		case a == "--emit-c" || a == "-c":
			emitC = true
		case strings.HasSuffix(a, ".nox"):
			if file != "" {
				return fmt.Errorf("usage: nox build [--emit-c]  |  nox build <file.nox> [--emit-c]")
			}
			file = a
		default:
			return fmt.Errorf("usage: nox build [--emit-c]  |  nox build <file.nox> [--emit-c]")
		}
	}
	if file != "" {
		return buildSingleFile(file, emitC)
	}
	return buildPackage()
}

// buildSingleFile compiles one .nox file given directly on the command
// line (not as part of a `nox init`-created package). Unlike package-mode
// builds, this does not create a build/ directory: the executable is
// written right next to the source file, and the intermediate C file is
// discarded after compiling unless emitC asks to keep it (also written
// next to the source file, as <stem>.c).
func buildSingleFile(path string, emitC bool) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	file, err := parser.Parse(string(data), path)
	if err != nil {
		return err
	}
	importRoot := filepath.Dir(path)
	if _, root, ok := pkgmgr.FindManifest(importRoot); ok {
		importRoot = root // still honor a nox.toml above it for import() resolution
	}
	stem := strings.TrimSuffix(filepath.Base(path), ".nox")
	return compileAndLink(file, importRoot, stem, filepath.Dir(path), emitC)
}

