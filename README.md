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

