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
