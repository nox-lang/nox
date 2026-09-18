# Nox

Nox is a statically-typed, natively-compiled programming language with strong
type inference. This is a compiler for it, written in Go: **Nox source →
generated C → native binary, compiled by
[tcc](https://bellard.org/tcc/) (Tiny C Compiler) and nothing else** — no
gcc, no clang, anywhere in the pipeline.

```
$ cat hello.nox
package main

import(
    "io"
)

func main() {
    io::println("Hello, World!")
}

$ nox build hello.nox
compiling -> hello (linux/amd64) via /usr/bin/tcc
built hello

$ ./hello
Hello, World!
```

> **Note on this README vs. `nox-spec.md`:** this repository also contains
> `nox-spec.md`, the original Nox specification as first given to build
> this compiler. Development continued past that document with a handful of
> direct amendments — most visibly, **standard-library function names are
> lowercase** (`io::println`, not `io::Println`; `parallel { }`, not
> `Parallel { }`) and **`return`/`next`/`yield` split apart** (see below) —
> that supersede what `nox-spec.md` shows. `nox-spec.md` is kept unmodified
> as the historical starting point; this README describes the language and
> compiler as actually implemented.

## Status

This implements essentially the full language: package/import/include, `let`
with type inference, arrays with all their methods, strings, functions
(defaults, variadics, closures), classes, `if` (as a statement *and* an
expression), `for`/`while`/`switch`, `break`/`next`/`yield`, `defer`,
`try`/`catch`/`?`, `async`/`await`/`parallel`, pointers, `.delete()`, the six
standard library packages, and the `nox` CLI (`init`/`build`/`get`) with
cross-compilation. It's been exercised with the programs under `examples/`
(see "What's been tested"), but it's a from-scratch implementation and has
**not** had long-term, adversarial testing — a solid, working first version,
not a hardened toolchain.

## Building the compiler itself

Requires Go 1.22+. No external Go modules are used (everything is standard
library), so it builds offline:

```
go build -o nox ./cmd/nox
```

Put the resulting `nox` binary on your `PATH`.

### Runtime toolchain requirements

- **[tcc](https://bellard.org/tcc/)** is the *only* C compiler this tool
  ever invokes — for every build, native or cross. `apt install tcc` on
  Debian/Ubuntu. See "Cross-compilation" below for targeting a different
  OS/architecture.
- **[Boehm GC](https://www.hboehm.info/gc/)** (`libgc`) — Nox's automatic
  memory management, for native builds. `apt install libtcc-dev libgc-dev`.
- **pthreads** — used for `async`/`await`/`parallel`, for native
  **non-Windows** builds only. On Windows, threading uses the Win32 API
  (`CreateThread`/`WaitForSingleObject`) directly — see "Threading and
  thread-local storage" below — so there is no pthread dependency there at
  all, not even indirectly through a pthreads-for-Windows shim.

`-lm` is always linked; `-lgc`/`-lpthread` are added only for a native
build (see `buildCompileCommand` in `cmd/nox/main.go`).

## Using the `nox` CLI

```
nox init <name>                  Scaffold a new package: ./<name>/nox.toml, ./<name>/src/main.nox
nox build                        Build the package in the current directory (nox.toml + src/) -> build/<name>
nox build <file.nox>             Build one file -> an executable next to it. No build/ directory,
                                  no .c file kept, unless...
nox build <file.nox> --emit-c    ...this is passed, which also writes <file>.c next to it.
nox get <source>                 Fetch a dependency (e.g. github.com/user/repo) via `git clone`
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
NOX_OS=windows NOX_ARCH=amd64 NOX_TCC=/path/to/windows-tcc nox build
```

Every build — native or cross — runs through tcc, selected by `NOX_TCC`
(default: `tcc` on `PATH`). There is no fallback to any other compiler,
ever. To cross-compile, point `NOX_TCC` at a tcc *build for that target* —
tcc's own project distributes separate cross-compiling builds (e.g. a
Windows-target tcc) distinct from the native Linux one `apt` installs; that
separate binary is what `NOX_TCC` should name. A cross build always compiles
with `-DNOX_NO_GC` (see "Threading and thread-local storage" / `runtime.c`)
since a target-matching build of Boehm GC isn't something this tool bundles
or can assume exists — this sandbox had no such Windows-target tcc binary
available to test that exact "point NOX_TCC at it" path end-to-end, so
treat cross-compilation as implemented-and-reasoned-through rather than
verified. What *was* verified in this environment: the generated C's
Windows-specific runtime code (the `#if defined(_WIN32)` branch in
`runtime.c` — Win32 threads, `TlsAlloc`-based TLS, `_mkdir`, etc.) compiles
cleanly and runs correctly under Wine when built with a Windows-target C
compiler by hand, confirming the *generated code itself* is genuinely
Windows-portable and pthread-free — independent of which specific compiler
binary ends up invoking it.

## Threading and thread-local storage

`async`/`await`/`parallel` and the per-thread error-propagation state (see
"Error handling" below) both need threads and thread-local storage.
`runtime.c` abstracts both behind small macros
(`NOX_THREAD_CREATE`/`NOX_THREAD_JOIN`/`NOX_TLS_*`) with two implementations
selected by `#if defined(_WIN32)`:

- **Windows**: native Win32 — `CreateThread`/`WaitForSingleObject` for
  threads, `TlsAlloc`/`TlsGetValue`/`TlsSetValue` for thread-local storage.
  `<pthread.h>` is never included on this path, and no pthreads
  implementation (bundled, static, or DLL) is a dependency of a Windows
  build at all.
- **Everywhere else**: plain POSIX pthreads (`pthread_create`/`pthread_join`,
  `pthread_key_t`). tcc does not support the `__thread` storage-class
  keyword, which is why thread-local storage goes through an explicit
  key/slot API on this path too, rather than a compiler-level thread-local
  variable.

Generated code (in `internal/codegen/async.go` and `closures.go`) only ever
emits the portable macro names, never a platform-specific call directly, so
the same generated `.c` file is what's compiled for every target.

## Repository layout

```
cmd/nox/                 CLI entry point (init/build/get, the tcc invocation)
internal/token/          Lexer token kinds
internal/lexer/          Hand-written lexer
internal/ast/            AST node definitions
internal/parser/         Recursive-descent parser
internal/codegen/        The compiler proper (AST -> C); see below
internal/runtime/c/      The C runtime prelude, embedded into every build
internal/pkgmgr/         nox.toml + `nox init`/`nox get`
examples/                Sample programs (see "What's been tested")
```

### How codegen works: monomorphization, not a type checker

The spec asks for "strong type inference" and shows both annotated and
*unannotated* function parameters (`func add(a, b)` and class fields typed
only by how a constructor happens to be called, e.g. `Dog.new("Pochi", 3)`
with no type anywhere in the class body). A conventional
Hindley-Milner-style inference pass doesn't fall out of that naturally when
combined with C as a backend — there's no polymorphism in C. Instead, this
compiler treats any function/class whose parameters aren't fully annotated
as an implicit generic, and **monomorphizes on demand**: the first time
`add(1, 2)` is seen, a concrete `add__i_i` is generated for `(int, int)`;
`add(1.5, 2.5)` elsewhere gets its own `add__f_f`. This is the same idea as
C++ templates or Zig's comptime generics, driven by the call graph starting
from `main`:

- A function that's never called (with any concrete types) is never
  emitted — dead-code elimination is a side effect of the architecture.
- **A genuinely recursive function whose return type isn't established
  before its first self-call needs an explicit return-type annotation**
  (`func fib(n): int { ... }`). If the base case's `return` is textually
  first, this isn't needed. If the recursive call comes first, the
  compiler asks for the annotation rather than silently producing
  something wrong.
- A class's field types are discovered by compiling `init` and watching for
  `this.field = ...` assignments — the type of the first assignment wins. A
  field never assigned in `init` needs an explicit type or a default value.
  An explicit type annotation on a *class* type (`let x: Dog`) only
  resolves if some `Dog.new(...)` call has already been monomorphized
  elsewhere in the program.

## `return` / `next` / `yield` / `break`: four distinct, non-overlapping jumps

The original spec overloads `return` with three different meanings
depending on where it's written (a plain return; "collect this value and
keep looping" inside `for`/`while`; implicitly, a callback's result inside
`each`/`map`/`filter`/`find`). Later direction explicitly asked for `return`
to mean the same single thing everywhere, like in most other languages, and
for the other two roles to get their own keywords. As implemented:

- **`return`** — always, unconditionally, exits the nearest enclosing
  *function* (or closure/async body) with a value, full stop — even from
  inside a `for`/`while` loop, even from inside an
  `each`/`map`/`filter`/`find` callback (callbacks passed as a literal
  lambda are inlined directly into the caller rather than compiled as a
  separate function — see below — so a `return` inside one really does exit
  the whole enclosing Nox function, not just that callback).
- **`next`** / **`next <value>`** — `for`/`while` loop control, like C's
  `continue`. Bare `next` just moves on to the next iteration. `next value`
  *also* collects `value` into an array that becomes the loop's own value
  when the loop is used as an expression (`let xs = for (...) { ... next
  y }`) — this is what `return value` used to do inside a loop, per the
  original spec's §11.1. `next` always targets the nearest enclosing real
  loop, skipping over (but not affected by) an intervening `switch`
  (switches never intercept it, matching how `break` does affect `switch`
  but `next`/`continue` conceptually shouldn't).
- **`yield <value>`** — used inside an `each`/`eachIndex`/`map`/`filter`/
  `find` callback (or a `.sort(...)` comparator) to supply that
  invocation's result, without exiting the enclosing function. This is what
  bare `return value` used to mean inside those callbacks.
- **`break`** / **`break <value>`** — unchanged from the spec: exits a loop
  or `switch`, optionally carrying a final value out as that construct's
  value when used as an expression.

A loop can't mix `next <value>` and `break <value>` (which "shape" would the
loop's value be, a collected array or a single break value?); that's a
compile error pointing at the ambiguity. `yield` outside a callback, or
`next` outside a loop, are compile errors too, not silent no-ops.

