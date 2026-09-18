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

func buildPackage() error {
	manifestPath, root, ok := pkgmgr.FindManifest(".")
	if !ok {
		return fmt.Errorf("no nox.toml found in this directory or any parent (try 'nox init <name>' or 'nox build <file.nox>')")
	}
	m, err := pkgmgr.Load(manifestPath)
	if err != nil {
		return err
	}
	srcDir := filepath.Join(root, "src")
	var noxFiles []string
	err = filepath.Walk(srcDir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(p, ".nox") {
			noxFiles = append(noxFiles, p)
		}
		return nil
	})
	if err != nil {
		return err
	}
	if len(noxFiles) == 0 {
		return fmt.Errorf("no .nox files found under %s", srcDir)
	}
	merged, err := parseAndMerge(noxFiles)
	if err != nil {
		return err
	}
	return compileAndLink(merged, root, m.Name, filepath.Join(root, "build"), true)
}

// parseAndMerge parses every file and merges their top-level declarations
// into one ast.File, as if they were one source file (Go-package-style
// multi-file compilation units), which is how the spec describes `nox
// build` handling multiple files under src/.
func parseAndMerge(files []string) (*ast.File, error) {
	merged := &ast.File{Package: "main", Filename: files[0]}
	seenFuncs := map[string]string{}
	seenClasses := map[string]string{}
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			return nil, err
		}
		pf, err := parser.Parse(string(data), f)
		if err != nil {
			return nil, err
		}
		if pf.Package != "" && merged.Package == "main" {
			merged.Package = pf.Package
		}
		for _, imp := range pf.Imports {
			merged.Imports = append(merged.Imports, imp)
		}
		for _, inc := range pf.Includes {
			merged.Includes = append(merged.Includes, inc)
		}
		for _, fn := range pf.Funcs {
			if prev, ok := seenFuncs[fn.Name]; ok {
				return nil, fmt.Errorf("function '%s' is defined in both %s and %s", fn.Name, prev, f)
			}
			seenFuncs[fn.Name] = f
			merged.Funcs = append(merged.Funcs, fn)
		}
		for _, cl := range pf.Classes {
			if prev, ok := seenClasses[cl.Name]; ok {
				return nil, fmt.Errorf("class '%s' is defined in both %s and %s", cl.Name, prev, f)
			}
			seenClasses[cl.Name] = f
			merged.Classes = append(merged.Classes, cl)
		}
		merged.Globals = append(merged.Globals, pf.Globals...)
	}
	return merged, nil
}

// ---------------- codegen -> C -> tcc ----------------

// compileAndLink generates C for file and compiles it with tcc. outputDir
// is where the binary (and, if keepC, the .c file) are written; when
// !keepC, the C file is written to a temporary location and removed once
// compilation finishes.
func compileAndLink(file *ast.File, projectRoot, outName string, outputDir string, keepC bool) error {
	cSource, err := codegen.Generate(file, noxruntime.Prelude, projectRoot)
	if err != nil {
		return err
	}

	if err := os.MkdirAll(outputDir, 0755); err != nil {
		return err
	}

	var cPath string
	if keepC {
		cPath = filepath.Join(outputDir, outName+".c")
		if err := os.WriteFile(cPath, []byte(cSource), 0644); err != nil {
			return err
		}
	} else {
		tmp, err := os.CreateTemp("", "nox-*.c")
		if err != nil {
			return err
		}
		cPath = tmp.Name()
		_, writeErr := tmp.WriteString(cSource)
		tmp.Close()
		if writeErr != nil {
			os.Remove(cPath)
			return writeErr
		}
		defer os.Remove(cPath)
	}

	targetOS := envOr("NOX_OS", runtime.GOOS)
	targetArch := envOr("NOX_ARCH", runtime.GOARCH)

	outPath := filepath.Join(outputDir, outName)
	if targetOS == "windows" {
		outPath += ".exe"
	}

	cmd := buildCompileCommand(targetOS, cPath, outPath)
	fmt.Printf("compiling -> %s (%s/%s) via %s\n", outPath, targetOS, targetArch, cmd.Path)
	out, err := cmd.CombinedOutput()
	if err != nil {
		if _, lookErr := exec.LookPath(cmd.Path); lookErr != nil && !filepath.IsAbs(cmd.Path) {
			return fmt.Errorf("'%s' was not found on PATH — install tcc, or point NOX_TCC at the tcc binary to use (e.g. a cross-compiling tcc build for %s)", cmd.Path, targetOS)
		}
		return fmt.Errorf("compilation failed:\n%s\n%v", string(out), err)
	}
	if len(strings.TrimSpace(string(out))) > 0 {
		fmt.Println(string(out))
	}
	if keepC {
		fmt.Printf("built %s (C source: %s)\n", outPath, cPath)
	} else {
		fmt.Printf("built %s\n", outPath)
	}
	return nil
}

func envOr(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

