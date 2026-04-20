// Package toolchain makes Nox fully self-contained: it builds (once, then
// caches) a private copy of nox-tcc from the source tree embedded in the
// nox binary (see /embed.go), so `nox build` never depends on a
// separately-installed tcc. It also extracts the embedded Nox runtime
// (include/, lib/) that every build compiles the generated program
// against.
//
// The only thing this package still asks of the host machine is *some* C
