package codegen

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"nox/internal/ast"
	"nox/internal/parser"
)

// resolveImports loads every `import(...)`-declared package from disk
// (relative to projectRoot, typically the directory containing nox.toml, or
// the entry file's directory for a single-file build) and registers it as a
// Namespace so `path::symbol` references resolve during codegen.
//
// An import path may name either a single file (`libs/math` ->
// `libs/math.nox`) or a directory of Nox files that are merged together as
// one namespace (`libs/math/` -> every `*.nox` file directly inside it,
// non-recursively). If no alias is given, the namespace is the import path
// itself with `/` replaced by `::` (matching the language spec).
func (cg *Codegen) resolveImports(projectRoot string) {
	for _, imp := range cg.file.Imports {
		nsKey := imp.Alias
		if nsKey == "" {
			nsKey = strings.ReplaceAll(imp.Path, "/", "::")
		}
		if _, exists := cg.namespaces[nsKey]; exists {
			// A stdlib package name (io, random, fs, path, math, time) can
			// be legally re-imported; leave the built-in registration in
			// place rather than overwriting it with a same-named user file.
			if _, isStd := map[string]bool{"io": true, "random": true, "fs": true, "path": true, "math": true, "time": true}[nsKey]; isStd {
				continue
			}
		}
		files, err := findImportFiles(projectRoot, imp.Path)
		if err != nil {
			panic(fmt.Sprintf("nox: import(\"%s\"): %s", imp.Path, err))
		}
		ns := &Namespace{Kind: NSUser, Funcs: map[string]*ast.FuncDecl{}, Classes: map[string]*ast.ClassDecl{}, Globals: map[string]*ast.LetStmt{}}
		for _, f := range files {
			data, err := os.ReadFile(f)
			if err != nil {
				panic(fmt.Sprintf("nox: import(\"%s\"): %s", imp.Path, err))
			}
			pf, err := parser.Parse(string(data), f)
			if err != nil {
				panic(fmt.Sprintf("nox: import(\"%s\"): %s", imp.Path, err))
			}
			for _, fn := range pf.Funcs {
				ns.Funcs[fn.Name] = fn
			}
			for _, cl := range pf.Classes {
				ns.Classes[cl.Name] = cl
			}
			for _, g := range pf.Globals {
				ns.Globals[g.Name] = g
			}
			// Classes/functions from imported files participate in the same
			// monomorphization engine as the main file; register them so
			// e.g. a class defined in an imported file can be
			// `.new()`-instantiated when referenced through the namespace.
			for name, fn := range ns.Funcs {
				if _, already := cg.funcsByName[name]; !already {
					cg.funcsByName[name] = fn
				}
			}
			for name, cl := range ns.Classes {
				if _, already := cg.classesByName[name]; !already {
					cg.classesByName[name] = cl
				}
			}
		}
		cg.namespaces[nsKey] = ns
	}
}

func findImportFiles(root, importPath string) ([]string, error) {
	base := filepath.Join(root, filepath.FromSlash(importPath))
	if st, err := os.Stat(base + ".nox"); err == nil && !st.IsDir() {
		return []string{base + ".nox"}, nil
	}
	if st, err := os.Stat(base); err == nil && st.IsDir() {
		entries, err := os.ReadDir(base)
		if err != nil {
			return nil, err
		}
		var files []string
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".nox") {
				files = append(files, filepath.Join(base, e.Name()))
			}
		}
		if len(files) == 0 {
			return nil, fmt.Errorf("directory '%s' contains no .nox files", base)
		}
		return files, nil
	}
	return nil, fmt.Errorf("cannot find '%s.nox' or a directory '%s' (looked under %s)", importPath, importPath, root)
}
