// Nox is fully self-contained: this file embeds everything the toolchain
// needs to build a program without any separately installed compiler or
// runtime files — the Nox standard library (include/, lib/) and the
// vendored nox-tcc source tree (tcc/) that internal/toolchain builds (once,
// then caches) into the tcc binaries `nox build` actually invokes.
package noxassets

import "embed"

// Runtime is the Nox C runtime: include/nox/nox.h (what every generated
// program #includes) plus lib/*.c (compiled alongside it). See both
// directories at the repository root for the human-readable source.
//
//go:embed all:include all:lib
var Runtime embed.FS

