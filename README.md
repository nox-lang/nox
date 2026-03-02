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

