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

