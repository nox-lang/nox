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
