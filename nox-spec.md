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

## 2.2 import

`import` はNoxのパッケージを読み込みます。

```text
import(
    "io",
    "libs/math" as math
)
```

パッケージパスには `/` を使用し、読み込んだ名前空間へのアクセスには `::` を使用します。

```text
math::add(1, 2)
```

エイリアスを指定しない場合はパスが名前空間になります。

```text
import("libs/math")
```

```text
libs::math::add(1, 2)
```

## 2.3 include

`include` はCヘッダを直接読み込みます。

```text
include(
    "stdio.h",
    "stdlib.h"
)
```

読み込んだC APIは名前空間からアクセスします。

```text
stdio::printf("Hello\n")
```

エイリアスも指定できます。

```text
include(
    "stdio.h" as cstdio
)
```

```text
cstdio::printf("Hello\n")
```

`import` はNoxパッケージ、`include` はCヘッダという明確な役割分担になっています。

# 3. 名前空間とメンバーアクセス

`::` は名前空間・パッケージ・ライブラリへのアクセスに使用します。

```text
io::Println("Hello")
math::sqrt(16)
```

`.` はオブジェクトや値のメンバーアクセスに使用します。

```text
dog.bark()
numbers.length
text.substring(0, 5)
```

# 4. 変数

変数の宣言には `let` を使用します。

```text
let name = "Tomoya"
let age = 14
```

型を明示することもできます。

```text
let age: int = 14
```

基本的には型推論が使用されます。

## 4.1 未初期化変数

```text
let value
```

初期値を持たない変数は `null` の状態になります。

ただし、プログラムから明示的に `null` を代入することはできません。

```text
value = null
```

これは禁止されます。

