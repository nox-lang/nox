# 1. Noxとは

> **Note (added after this document was written):** development continued
> past this original specification with a few direct amendments from
> later instructions — most visibly, standard library function names are
> lowercase (`io::println`, not `io::Println`; `parallel { }`, not
> `Parallel { }`), and `return`/`next`/`yield` were split into three
> distinct keywords instead of `return` being overloaded to mean three
> different things by context. This file is kept exactly as originally
> given, for the historical record; see `README.md` for the language and
> compiler as actually implemented.

Noxは、静的型付けと強力な型推論を持つネイティブ向けプログラミング言語です。

基本的なコードはシンプルに記述でき、必要に応じてC言語との直接的な連携も可能です。

ソースファイルの拡張子は `.nox` です。

Noxでは、基本的にNox自身の機能やNoxパッケージを使用し、どうしてもCを利用する必要がある場合にCヘッダを直接読み込みます。

# 2. 基本構文

## 2.1 package

`package` はファイルの名前空間を定義します。

ファイル全体を `{}` で囲む必要はありません。

```text
package main

import(
    "io"
)

func main() {
    io::Println("Hello, World!")
}
```

