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

