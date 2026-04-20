// Package toolchain makes Nox fully self-contained: it builds (once, then
// caches) a private copy of nox-tcc from the source tree embedded in the
// nox binary (see /embed.go), so `nox build` never depends on a
// separately-installed tcc. It also extracts the embedded Nox runtime
// (include/, lib/) that every build compiles the generated program
// against.
//
// The only thing this package still asks of the host machine is *some* C
// compiler (cc, gcc, or clang — nearly universal on Linux/macOS, and
// available on Windows via MSVC or MinGW) to bootstrap nox-tcc itself, the
// first time it's needed; after that first build, everything is cached and
// the host compiler is never invoked again.
package toolchain

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"

	noxassets "nox"
)

// cacheVersion is bumped whenever the embedded tcc/runtime sources change
// in a way that requires a rebuild; it is folded into the cache directory
// name so an old cached build is never mistakenly reused after a nox
// upgrade.
const cacheVersion = "v1"

// Toolchain is a ready-to-use, on-disk nox-tcc + Nox runtime, extracted and
// (for whichever targets have actually been requested) built.
type Toolchain struct {
	root        string // cache root: <UserCacheDir>/nox/toolchain-<version>
	tccSrcDir   string // root/tcc — the vendored nox-tcc tree (also the native build dir)
	runtimeDir  string // root/runtime — include/ + lib/
	builtNative bool
	builtWin32  bool
}

// RuntimeIncludeDir is where <nox/nox.h> lives.
func (tc *Toolchain) RuntimeIncludeDir() string { return filepath.Join(tc.runtimeDir, "include") }

// RuntimeSources returns every lib/*.c file to compile alongside the
// generated program.
func (tc *Toolchain) RuntimeSources() ([]string, error) {
	entries, err := os.ReadDir(filepath.Join(tc.runtimeDir, "lib"))
	if err != nil {
		return nil, err
	}
	var out []string
	for _, e := range entries {
		if !e.IsDir() && filepath.Ext(e.Name()) == ".c" {
			out = append(out, filepath.Join(tc.runtimeDir, "lib", e.Name()))
		}
	}
	return out, nil
}

// New extracts the embedded runtime (always) into a per-user cache
// directory, ready for EnsureNative / EnsureWindowsCross to build nox-tcc
// into. Extraction itself needs no C compiler and always happens.
func New() (*Toolchain, error) {
	base, err := os.UserCacheDir()
	if err != nil {
		base = os.TempDir()
	}
	root := filepath.Join(base, "nox", "toolchain-"+cacheVersion)
	tc := &Toolchain{
		root:       root,
		tccSrcDir:  filepath.Join(root, "tcc"),
		runtimeDir: filepath.Join(root, "runtime"),
	}
	if err := extractOnce(tc.runtimeDir, "include-lib", func(dst string) error {
		return extractFS(noxassets.Runtime, dst)
	}); err != nil {
		return nil, fmt.Errorf("extracting the Nox runtime: %w", err)
	}
	if err := extractOnce(tc.tccSrcDir, "tcc-src", func(dst string) error {
