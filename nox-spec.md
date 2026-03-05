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

# 5. 型

基本型として以下を持ちます。

```text
int
float
bool
string
```

配列型にはジェネリック形式を使用します。

```text
array<int>
array<string>
```

Noxでは暗黙の型変換を行いません。

必要な場合は明示的な変換を行います。

```text
value.toInt()
value.toFloat()
value.toString()
value.toBool()
```

# 6. 配列

配列は `[]` で記述します。

```text
let numbers = [1, 2, 3, 4, 5]
```

明示的な型指定も可能です。

```text
let numbers: array<int> = []
```

多次元配列にも対応します。

```text
let matrix = [
    [1, 2],
    [3, 4]
]
```

## 6.1 基本操作

```text
numbers.length
numbers[0]
numbers[0] = 100

numbers.push(6)
numbers.pop()
numbers.insert(1, 50)
numbers.remove(1)
numbers.clear()
```

## 6.2 each

`each` は要素だけを受け取ります。

```text
numbers.each((x) {
    io::Println(x)
})
```

## 6.3 eachIndex

`eachIndex` はインデックスと値を受け取ります。

```text
numbers.eachIndex((index, value) {
    io::Printfn("{}: {}", index, value)
})
```

## 6.4 eachLine

文字列を1行ずつ処理できます。

```text
text.eachLine((line) {
    io::Println(line)
})
```

## 6.5 map

`map` は新しい配列を返します。

```text
let doubled = numbers.map((x) {
    return x * 2
})
```

## 6.6 filter

条件に一致する要素から新しい配列を作ります。

```text
let result = numbers.filter((x) {
    return x > 10
})
```

## 6.7 find

条件に一致する最初の要素を取得します。

```text
let result = numbers.find((x) {
    return x > 10
})
```

見つからなかった場合の詳細な仕様は今後決定します。

## 6.8 sort

```text
numbers.sort()
```

比較処理を指定する形式も用意できます。

```text
numbers.sort((a, b) {
    return a < b
})
```

## 6.9 reverse

`reverse` は新しい配列を返します。

```text
let reversed = numbers.reverse()
```

# 7. 文字列

文字列では以下の操作を使用できます。

```text
text.length
text.empty()
text.contains("abc")
text.startsWith("abc")
text.endsWith("abc")
text.substring(0, 5)
```

文字列の連結には `+` を使用します。

```text
let message = "Hello, " + name
```

# 8. 関数

関数は `func` で定義します。

```text
func add(a, b) {
    return a + b
}
```

型を明示することもできます。

```text
func add(a: int, b: int): int {
    return a + b
}
```

## 8.1 デフォルト引数

```text
func greet(name = "World") {
    io::Println(name)
}
```

## 8.2 可変長引数

```text
func sum(values...) {
    ...
}
```

## 8.3 無名関数

```text
let double = (x) {
    return x * 2
}
```

# 9. if

```text
if (condition) {
    ...
} else if (condition) {
    ...
} else {
    ...
}
```

`if` は暗黙的に値を返しません。

値を返す場合は `return` を使用します。

```text
func check(x) {
    if (x > 10) {
        return x
    }

    return 0
}
```

