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
