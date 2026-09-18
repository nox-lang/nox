// Package pkgmgr implements the small subset of package-manifest handling
// (nox.toml) and project scaffolding described in the Nox language spec:
// `nox init`, `nox get`, and reading dependency declarations for `nox
// build`. It intentionally implements just enough of a TOML-like format for
// this one file shape — it is not a general TOML parser.
package pkgmgr

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
)

