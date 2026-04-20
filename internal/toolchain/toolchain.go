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
		return extractFSSub(noxassets.TCCSource, "tcc", dst)
	}); err != nil {
		return nil, fmt.Errorf("extracting the bundled nox-tcc source: %w", err)
	}
	return tc, nil
}

// marker-file based idempotency: re-extract only if the marker is missing
// (e.g. first run, or a cache wipe), never on every invocation.
func extractOnce(dst, tag string, extract func(dst string) error) error {
	marker := filepath.Join(dst, ".nox-extracted-"+tag)
	if _, err := os.Stat(marker); err == nil {
		return nil
	}
	if err := os.RemoveAll(dst); err != nil {
		return err
	}
	if err := os.MkdirAll(dst, 0755); err != nil {
		return err
	}
	if err := extract(dst); err != nil {
		return err
	}
	return os.WriteFile(marker, []byte("ok\n"), 0644)
}

// hostCC finds a C compiler to bootstrap nox-tcc with. Any of these is
// virtually always present on a machine that does any native development.
func hostCC() (string, error) {
	for _, cand := range []string{"cc", "gcc", "clang"} {
		if p, err := exec.LookPath(cand); err == nil {
			return p, nil
		}
	}
	return "", fmt.Errorf("no C compiler found (tried cc, gcc, clang) — nox needs one, just once, to build its bundled nox-tcc; install one (e.g. 'apt install gcc', 'xcode-select --install', or a Visual Studio/MinGW toolchain on Windows) and try again")
}

func (tc *Toolchain) configured() bool {
	_, err := os.Stat(filepath.Join(tc.tccSrcDir, "config.mak"))
	return err == nil
}

func (tc *Toolchain) configure() error {
	if tc.configured() {
		return nil
	}
	cc, err := hostCC()
	if err != nil {
		return err
	}
	cmd := exec.Command("./configure", "--cc="+cc)
	cmd.Dir = tc.tccSrcDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("configuring the bundled nox-tcc failed:\n%s\n%w", out, err)
	}
	return nil
}

// EnsureNative builds (if not already cached) a native tcc for this host,
// and returns its path plus the -B search directory it needs (for
// libtcc1.a).
func (tc *Toolchain) EnsureNative() (tccPath, searchDir string, err error) {
	tccPath = filepath.Join(tc.tccSrcDir, "tcc")
	if runtime.GOOS == "windows" {
		tccPath += ".exe"
	}
	if _, statErr := os.Stat(tccPath); statErr != nil {
		if err := tc.configure(); err != nil {
			return "", "", err
		}
		cmd := exec.Command("make", "-j"+strconv.Itoa(runtime.NumCPU()))
		cmd.Dir = tc.tccSrcDir
		out, err := cmd.CombinedOutput()
		if err != nil {
