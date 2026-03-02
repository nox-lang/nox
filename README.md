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
