# nox-tcc: tcc + bundled BDWGC

Both tcc targets this checkout can produce are bundled with an actual
[BDWGC](https://github.com/ivmai/bdwgc) build **compiled by tcc itself**
(not gcc/mingw — tcc's own PE/ELF object reader rejects gcc-family object
files outright, so mingw-built objects cannot be linked by tcc at all; this
had to be a genuine tcc-native build).

## Linux (native)

`dist/bin/tcc` + `dist/lib/tcc/{gc.h, gc/*.h, libgc.a, ...}`. Just:

    dist/bin/tcc yourprog.c -o yourprog -lgc -lpthread -lm

Static-only in `dist/lib/tcc/` (deliberately no `.so` there) so a bare
`-lgc` can't be silently satisfied by some unrelated libgc.so a target
machine happens to already have installed — verified with `ldd` that the
result has no libgc runtime dependency at all.

## Windows (win32/)

`win32/include/gc.h` + `win32/include/gc/*.h` (fixes the reported
`gc/gc.h not found` — the previous drop only had the top-level stub, not
the real header directory), plus tcc's own compiler-builtin headers
(`stddef.h` etc. — win32/include didn't have these at all, needed
regardless of GC) and `win32/lib/libgc.a`.

**This libgc.a is a real, from-source BDWGC build, compiled with tcc's own
win32 cross-compiler** (`make cross-x86_64-win32`, which uses the exact
same `win32/include` + `win32/lib` search paths as a native `win32/tcc.exe`
— i.e. testing with it is equivalent to testing with what you have).
Verified end-to-end under Wine: compiles, links with a bare `-lgc`, runs,
correct output, single-threaded allocation confirmed working.

Getting a tcc-buildable BDWGC required several targeted fixes on top of
upstream source (see `git log` in this checkout for each one individually):
tcc doesn't define `__GNUC__` (misdirects BDWGC's MSVC-suffix numeric-
literal branch — fixed by defining it), doesn't implement SEH (`__try` —
disabled via `NO_SEH_AVAILABLE`), and implements only
`__atomic_compare_exchange_n` of the `__atomic_*` builtin family, not
`__atomic_load_n`/`__atomic_store_n`/`__atomic_signal_fence` (worked around
with function-like macro definitions, since those three only ever need a
plain load/store/no-op on x86-64, not the version-specific instructions
the real builtins can involve).

### Known limitation: multi-threaded GC registration crashes

A **single-threaded** program (`GC_INIT()`, `GC_MALLOC()`, no other
threads) works correctly and was verified running under Wine. A program
that spawns a thread and calls `GC_register_my_thread()` from it — i.e.
exactly what Nox's own `async`/`await`/`parallel` needs — currently
**crashes** (NULL-pointer read) somewhere in BDWGC's win32 thread-
registration path when built this way. Root cause not yet isolated (it is
specific to this tcc-compiled build; the same C source builds and runs
correctly for the single-threaded case, so it is not a wrong header/define
— something in the thread-registration path miscompiles or a struct
layout assumption doesn't hold under tcc specifically). **If you use
`async`/`await`/`parallel` on Windows with this tcc, build with
`-DNOX_NO_GC` for now** (plain `malloc`, no collection — correct, just not
garbage-collected) until this is root-caused; everything else is fine with
real GC. This is flagged here rather than silently shipped as "done" —
it's a genuine open problem, not a rounding error.

## Nox-side integration

`nox`'s own `cmd/nox/main.go` (`bundledGCStaticLib`) auto-detects either
layout above next to whatever `NOX_TCC` points at, and links the static
`libgc.a` by explicit path rather than a bare `-lgc` — see that function's
own comment for why a bare `-lgc` isn't reliable even when a bundled
static lib is sitting right there.
