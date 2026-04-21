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

