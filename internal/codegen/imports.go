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
