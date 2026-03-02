# Nox

Nox is a statically-typed, natively-compiled programming language with strong
type inference. This is a compiler for it, written in Go: **Nox source →
generated C → native binary, compiled by nox-tcc (a bundled, from-source
copy of [tcc](https://bellard.org/tcc/), see "The bundled toolchain" below)
and nothing else** — no gcc, no clang, anywhere in the generated program's
own compilation, and (after the very first run on a machine) no separately
installed tcc either.

```
$ cat hello.nox
package main

import(
    "io"
)

func Main() {
    io::Println("Hello, World!")
}

$ nox build hello.nox
compiling -> hello (linux/amd64) via bundled nox-tcc
built hello

$ ./hello
Hello, World!
```

> **Note on this README vs. `nox-spec.md`:** this repository also contains
> `nox-spec.md`, the original Nox specification as first given to build
> this compiler. Development continued past that document in two rounds of
> direct amendments that supersede what `nox-spec.md` shows:
>
> - The original round: **`return`/`next`/`yield` split apart** (see
>   below) and, at the time, lowercase standard-library names — since
>   superseded again by the round below.
> - **A 2026 redesign — see "2026 redesign" below** for the full list:
>   `private` was removed in favor of Go-style capitalization-is-visibility
>   everywhere (including the standard library, and the entry point, now
>   `func Main()`); `type` aliases; a real `[]T` slice / `[N]T` array
>   distinction plus `make`/`range`/associative arrays (`map<K, V>`); `_`;
>   `static` class members; `Thread`/`Task` (alongside the still-present
>   `async`/`await`/`parallel`); nox-tcc fully bundled into `nox` itself;
>   and `nox get` deferring its `git clone` to the next `nox build`.
>
> `nox-spec.md` is kept unmodified as the historical starting point; this
> README describes the language and compiler as actually implemented.

## Status

This implements essentially the full language: package/import/include, `let`
with type inference, slices *and* fixed-size arrays with all their methods
(plus `make`/`range`/associative arrays), strings, functions (defaults,
variadics, closures), classes (including `static` members), `if` (as a
statement *and* an expression), `for`/`while`/`switch`, `break`/`next`/`yield`,
`defer`, `try`/`catch`/`?`, `async`/`await`/`parallel` *and* the explicit
`Thread`/`Task` API, pointers, `.delete()`, the six standard library
packages, `type` aliases, and the `nox` CLI (`init`/`build`/`get`) with
cross-compilation via a fully bundled nox-tcc. It's been exercised with the
programs under `examples/` (see "What's been tested"), but it's a
from-scratch implementation and has **not** had long-term, adversarial
testing — a solid, working first version, not a hardened toolchain.

## 2026 redesign

A later pass through the whole compiler changed several load-bearing pieces
of the language and toolchain at once:

- **`private` is gone.** Visibility is now Go-style and automatic:
  anything — a top-level `func`/`let`/`class`, or a class field/method —
  whose name starts with an uppercase letter is public; a lowercase-initial
  name is private to its file (top level) or its class (members). The
  standard library follows the same rule, so it now reads `io::Println`,
  `fs::Read`, `math::Sqrt`, and so on (package names themselves —
  `io`, `fs`, `math` — stay lowercase, exactly like a Go package name).
  The program's entry point is now capitalized too: **`func Main()`**, not
  `func main()`. `private` itself is a reserved word that now always
  produces a compile error pointing at this.
- **`type Name = T`** declares a transparent type alias (Go-style),
  anywhere a type could otherwise be written.
- **Slices and arrays are distinct types.** `[]T` is a slice (a growable,
  reference-like view — what "array" meant before); `[N]T` is a genuine
  fixed-size array, copied by value like a small struct. `make([]T, len[,
  cap])` allocates a slice with spare capacity up front; `s[lo:hi]` slices
  either one (and a `string`); `s.length`/`s.capacity` read a slice's
  length/capacity, `a.length` an array's (a compile-time constant).
- **Associative arrays**: `map<K, V>`, with a literal form (`{k: v, ...}`
  or the explicit `map<K, V>{...}`), `make(map<K, V>)`, `m[k]` /
  `m[k] = v`, `.length`, and `for (k, v in m)` (insertion-ordered).
- **`for (v in range(lo, hi[, step]))`** (and `range(hi)`, `range(lo, hi)`)
  — a direct counting loop, compiled without ever materializing a slice.
- **The blank identifier `_`** discards a value — `let _ = f()`,
  `for (_, v in xs)` — exactly like Go's.
- **`static`** on a class field or method makes it a class-level member —
  one `Counter.Total` shared by every instance, not per-object — reached
  as `ClassName.member` from anywhere (subject to the same
  capitalization-is-visibility rule).
- **`Thread`/`Task`**, alongside the pre-existing `async`/`await`/
  `parallel` (both now share one underlying runtime, and freely
  interoperate): `Thread.new(() { ... })` / `.Start()` / `.Join()` for a
  plain thread; `Task.Run(() { ... })` / `.Result` / `Task.WhenAll(tasks)`
  for a C#-`Task`-flavored alternative to `async func`+`await`.
- **nox-tcc is now bundled inside `nox` itself** — see "The bundled
  toolchain" below — rather than being a separately installed `tcc`.
- **`nox get` no longer clones immediately.** It only records the
  dependency in `nox.toml`; the actual `git clone` happens the next time
  `nox build` runs (reading `nox.toml`), matching how `nox get` reads
  elsewhere as "declare a dependency", not "fetch it right now".

## Building the compiler itself

Requires Go 1.22+. No external Go modules are used (everything is standard
library), so it builds offline:

```
go build -o nox ./cmd/nox
```

Put the resulting `nox` binary on your `PATH`.

### The bundled toolchain

`nox` no longer depends on a separately installed tcc. The `tcc/` directory
at the repository root is a **from-source-only** copy of nox-tcc
(github.com/nox-lang/nox) — no prebuilt binaries — embedded straight into
the `nox` binary (`embed.go`, `internal/toolchain`). The first time a build
actually needs it, `nox`:

1. extracts that source tree into a per-user cache directory
   (`os.UserCacheDir()/nox/toolchain-v1/`);
2. runs its `./configure` + `make` (for a native build) or
   `make cross-x86_64-win32` (the first time a Windows target is built —
   tcc cross-builds itself, see `tcc/GC_BUNDLE.md`) there, once;
3. every later build reuses that cached binary directly.

The Nox standard library itself lives in **`include/`** (`nox/nox.h`, what
every generated program `#include`s) and **`lib/`** (`nox_*.c`, compiled
alongside the generated program) at the repository root, also embedded and
extracted the same way. Both are ordinary, readable files — nothing about
the embedding changes what's in them.

The only thing this still asks of the *host* machine is **some C compiler**
(`cc`, `gcc`, or `clang` — tried in that order) to perform that one-time
bootstrap in step 2; after that, the host compiler is never invoked again.
`NOX_TCC=/path/to/tcc` remains available as an escape hatch to bypass the
bundled toolchain entirely and use a specific external tcc binary instead.

- **[Boehm GC](https://www.hboehm.info/gc/)** (`libgc`) — Nox's automatic
  memory management, linked for a native (non-Windows-target) build only.
  `apt install libgc-dev` on Debian/Ubuntu (needed on the *build* machine
  only, to link against — not needed at all for a Windows-target build,
  cross or native, which uses a non-collecting allocator instead; see
  `include/nox/nox.h`'s `NOX_USE_GC`).
- **pthreads** — linked for `async`/`await`/`parallel`/`Thread`/`Task`, for
  a native non-Windows-target build only. A Windows target (cross-compiled
  from Linux/macOS, or built natively on Windows) uses the Win32 API
  (`CreateThread`/`WaitForSingleObject`) directly instead — see "Threading
  and thread-local storage" below — so there is no pthread dependency
  there at all.

`-lm`/`-lgc`/`-lpthread` are added only for a non-Windows *target* (see
`buildCompileCommand` in `cmd/nox/main.go`); a Windows target links against
nothing beyond what tcc's win32 `.def`-based import stubs already provide.

## Using the `nox` CLI

```
nox init <name>                  Scaffold a new package: ./<name>/nox.toml, ./<name>/src/main.nox
nox build                        Build the package in the current directory (nox.toml + src/) -> build/<name>
                                  Dependencies declared via 'nox get' are cloned here first, if missing.
nox build <file.nox>             Build one file -> an executable next to it. No build/ directory,
                                  no .c file kept, unless...
nox build <file.nox> --emit-c    ...this is passed, which also writes <file>.c next to it.
nox get <source>                 Declare a dependency (e.g. github.com/user/repo) in nox.toml.
                                  This does NOT clone it — that happens on the next 'nox build',
                                  which reads nox.toml for what to fetch (see internal/pkgmgr's
                                  EnsureDeps). 'nox get' alone only ever edits nox.toml.
```

The distinction is deliberate: `nox build` alone only makes sense for a
package that has a `nox.toml` (created by `nox init`), and always produces
`build/<package-name>` (plus `build/<package-name>.c`), matching the spec's
description of multiple files under `src/` becoming one executable.
`nox build <file.nox>` is the lightweight, no-ceremony path for a single
file — by default it leaves nothing behind but the executable itself.

`import("some/path")` pulls in *another* package: resolved relative to the
project root (the directory with `nox.toml`, or the entry file's directory
for `nox build <file.nox>`), as either `some/path.nox` or a directory
`some/path/` of `.nox` files. Per the spec, the unaliased path becomes the
`::`-namespace (`import("libs/math")` → `libs::math::add(...)`); an alias
shortens it (`import("libs/math") as m` → `m::add(...)`).

### Cross-compilation

```
NOX_OS=windows NOX_ARCH=amd64 nox build
```

That's it — no separate tcc build to go find and point at. The first time a
Windows target is requested on a given machine, `nox` bootstraps tcc's own
`x86_64-win32` cross target from the same bundled source (`make
cross-x86_64-win32`; tcc can cross-build itself, per `tcc/GC_BUNDLE.md`)
and caches it alongside the native one, entirely automatically. `NOX_TCC`
still works as a manual override if you'd rather supply your own tcc build
for the target.

A Windows target — cross-compiled from Linux/macOS, or built natively on
Windows — always uses the non-collecting allocator instead of Boehm GC (see
`include/nox/nox.h`'s `NOX_USE_GC`), sidestepping a real, known crash:
multi-threaded GC *registration* segfaults on a tcc-compiled Windows binary
(see `tcc/GC_BUNDLE.md`), so this isn't a placeholder pending a real fix —
it's the actual fix. This was verified end-to-end in this environment: a
program exercising slices, arrays, `map`, classes with `static` members,
`Thread`/`Task`, and `async`/`await`/`parallel` together was cross-compiled
with the bundled toolchain and produced byte-identical output running
under Wine to the same program built natively for Linux.

## Threading and thread-local storage

`async`/`await`/`parallel` and the per-thread error-propagation state (see
"Error handling" below) both need threads and thread-local storage.
`runtime.c` abstracts both behind small macros
(`NOX_THREAD_CREATE`/`NOX_THREAD_JOIN`/`NOX_TLS_*`) with two implementations
