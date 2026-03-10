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

# 10. ループ

## 10.1 条件式for

```text
for (condition) {
    ...
}
```

## 10.2 配列for

```text
for (value in array) {
    ...
}
```

インデックスと値を同時に取得することもできます。

```text
for (index, value in numbers) {
    ...
}
```

## 10.3 while

```text
while (condition) {
    ...
}
```

# 11. breakとループの戻り値

通常の `break` はループを終了します。

```text
break
```

`break value` はループを終了し、値をループの結果として返します。

```text
let result = for (x in numbers) {
    if (x > 10) {
        break x
    }
}
```

## 11.1 ループ内のreturn

ループ内の `return value` はループを終了しません。

値を集め、ループ終了時に配列として結果を返します。

```text
let results = for (x in numbers) {
    if (x > 10) {
        return x
    }
}
```

例えば、

```text
[5, 12, 20, 30]
```

から実行すると、

```text
[12, 20, 30]
```

になります。

つまり、

```text
break value
```

は「ループを終了して1つの値を返す」、

```text
return value
```

は「ループを継続しながら値を収集する」

という違いがあります。

# 12. switch

```text
switch (value) {
    case 1 {
        io::Println("one")
    }
    case 2 {
        io::Println("two")
    }
    default {
        io::Println("other")
    }
}
```

複数の値を1つのcaseで扱うこともできます。

```text
switch (value) {
    case 1, 2, 3 {
        ...
    }
    default {
        ...
    }
}
```

caseは自動的に終了します。

## 12.1 switchの戻り値

`break value` を使用して値を返します。

```text
let result = switch (value) {
    case 1 {
        break "one"
    }
    case 2 {
        break "two"
    }
    default {
        break "other"
    }
}
```

# 13. クラス

クラスは `class` で定義します。

```text
class Dog {
    let name
    let age

    func init(name, age) {
        this.name = name
        this.age = age
    }

    func bark() {
        io::Println("Woof!")
    }
}
```

インスタンス生成には `.new()` を使用します。

```text
let dog = Dog.new("Pochi", 3)
```

`.new()` に渡された引数は `init` に渡されます。

メンバーアクセスには `.` を使用します。

```text
dog.bark()
dog.name
```

`this` が現在のインスタンスを表します。

## 13.1 private

通常の宣言はpublicです。

必要な場合は `private` を使用します。

```text
private let id = 123

private func secret() {
    ...
}
```

`private` はクラス内だけでなく、パッケージレベルの宣言にも使用できます。

# 14. コマンドライン引数

`main` に `args` を指定するとコマンドライン引数を取得できます。

```text
func main(args) {
    ...
}
```

`args` の型は、

```text
array<string>
```

です。

実行ファイル名は含まれません。

例えば、

```text
app hello world
```

の場合、

```text
args[0] == "hello"
args[1] == "world"
```

となります。

# 15. 標準ライブラリ

Noxの標準ライブラリは必要以上に巨大化させず、以下の6つを基本とします。

```text
io
random
fs
path
math
time
```

## 15.1 io

```text
io::Print(...)
io::Println(...)
io::Printf(...)
io::Printfn(...)

io::Scan(...)
io::Scanln(...)
io::Scanf(...)
```

## 15.2 random

```text
random::rand()
random::rand(min, max)

random::randf()
random::randf(min, max)

random::choice(array)
random::shuffle(array)
```

## 15.3 fs

```text
fs::read(path)
fs::write(path, data)
fs::append(path, data)

fs::exists(path)
fs::remove(path)
fs::rename(old, new)
fs::copy(src, dest)

fs::mkdir(path)
fs::rmdir(path)
fs::list(path)
```

`fs::read()` の戻り値は `string` です。

## 15.4 path

```text
path::join(...)
path::basename(path)
path::dirname(path)
path::ext(path)
path::stem(path)
path::absolute(path)
```

## 15.5 math

```text
math::abs(x)
math::min(a, b)
math::max(a, b)
math::pow(x, y)
math::sqrt(x)

math::floor(x)
math::ceil(x)
math::round(x)

math::sin(x)
math::cos(x)
math::tan(x)

math::asin(x)
math::acos(x)
math::atan(x)

math::log(x)
math::log10(x)
math::exp(x)

math::PI
math::E
```

## 15.6 time

```text
time::now()
time::unix()
time::sleep(seconds)
time::clock()

time::year(t)
time::month(t)
time::day(t)
time::hour(t)
time::minute(t)
time::second(t)
```

# 16. async / await

`async` と `await` は言語組み込み機能です。

```text
async func fetchData() {
    return "Hello"
}

func main() {
    let data = await fetchData()
    io::Println(data)
}
```

非同期処理を変数に保存してからawaitすることもできます。

```text
let task = fetchData()
let data = await task
```

複数の処理を先に開始できます。

```text
let a = fetchA()
let b = fetchB()

let resultA = await a
let resultB = await b
```

# 17. Parallel

複数の非同期処理を並列に実行するための組み込み機能として `Parallel` を持ちます。

基本形は、

```text
let results = await Parallel {
    fetchA()
    fetchB()
    fetchC()
}
```

です。

結果は処理順を維持した配列として取得します。

```text
let a = results[0]
let b = results[1]
let c = results[2]
```

詳細な仕様は今後調整します。

# 18. エラー処理

Nox内部には `Result` の仕組みがあります。

ただし、Rustのように常に `Result<T, E>` を明示する必要はありません。

## 18.1 `?`

`?` はエラーを呼び出し元へ伝播させます。

```text
func load(): string {
    let text = fs::read("hello.txt")?
    return text
}
```

## 18.2 try / catch

エラーを処理する場合は `try / catch` を使用します。

```text
try {
    let text = load()
    io::Println(text)
} catch (error) {
    io::Println(error)
}
```

# 19. defer

`defer` は現在の関数を終了する直前に処理を実行します。

```text
func test() {
    defer {
        io::Println("cleanup")
    }

    io::Println("work")
}
```

実行順は、

```text
work
cleanup
```

となります。

`return` で関数を終了する場合も、終了前に `defer` が実行されます。

`defer` はGCの代わりではなく、明示的な後処理に使用します。

# 20. メモリ管理

Noxは**基本的にGC（ガベージコレクション）による自動メモリ管理**を採用します。

```text
let numbers = [1, 2, 3]
let text = "Hello"
```

のような値について、不要になったメモリはGCによって回収されます。

一方、Cを `include` して直接利用する場合は、C側のメモリ管理規則に従います。

# 21. ポインタ

Noxではポインタを扱うことができます。

基本的なポインタ型は、

```text
pointer<int>
```

のように記述します。

GCによる自動メモリ管理を基本としながら、C連携などの低レベル処理ではポインタを利用できます。

# 22. C連携

Noxでは、基本的にはDLLやSOなどの動的ライブラリに直接依存する設計を避けます。

どうしてもCを利用したい場合は、Cヘッダを `include` で直接読み込みます。

```text
include(
    "stdio.h"
)
```

C APIはNoxから直接利用できます。

```text
stdio::printf("Hello\n")
```

つまり、

```text
Nox
├── 標準ライブラリ
├── Noxパッケージ → import
└── Cを直接利用 → include
```

という構成です。

# 23. パッケージ管理

Noxには `nox` コマンドを用意します。

## 23.1 パッケージ初期化

```text
nox init hello
```

新しいNoxパッケージを作成します。

基本的な構成は、

```text
hello/
├── nox.toml
└── src/
    └── main.nox
```

です。

## 23.2 パッケージ取得

```text
nox get github.com/rimsky-yamatov/noxlib
```

取得したパッケージは `nox.toml` に依存関係として記録されます。

例えば、

```toml
[package]
name = "hello"
version = "0.1.0"

[dependencies]
noxlib = "github.com/rimsky-yamatov/noxlib"
```

のようになります。

取得したNoxパッケージは、Noxコードから `import` して利用します。

# 24. nox.toml

最小構成は、

```toml
[package]
name = "hello"
version = "0.1.0"

[dependencies]
```

です。

`[package]` にはパッケージ名とバージョンを指定します。

`[dependencies]` には外部Noxパッケージを指定します。

# 25. ビルド

## 25.1 パッケージ全体

```text
nox build
```

`nox init` で作成したパッケージ全体をビルドします。

複数の `.nox` ファイルが存在していても、基本的に**1つの実行ファイル**として出力されます。

```text
hello/
├── nox.toml
└── src/
    ├── main.nox
    ├── math.nox
    └── util.nox
```

↓

```text
build/
└── hello.exe
```

## 25.2 ファイル単体

```text
nox build hello.nox
```

とした場合は `hello.nox` 単体をビルドします。

## 25.3 クロスビルド

環境変数によって対象OSを指定できます。

```text
NOX_OS=windows nox build
```

```text
NOX_OS=linux nox build
```

CPUアーキテクチャも指定できます。

```text
NOX_OS=linux NOX_ARCH=amd64 nox build
```

```text
NOX_OS=linux NOX_ARCH=arm64 nox build
```

環境変数を指定しなかった場合は、現在の環境を対象とします。

# 26. 現在のNoxの全体像

Noxの基本的な設計は以下のようになります。

```text
                    Nox
                     │
        ┌────────────┼────────────┐
        │            │            │
     Nox標準       Noxパッケージ    C
        │            │            │
  io/random/fs     import       include
  path/math/time
        │            │            │
        └────────────┼────────────┘
                     │
                  nox build
                     │
               1つの実行ファイル
```

言語自体は静的型付け・強い型推論を基本とし、GCによる自動メモリ管理を採用します。

必要に応じてポインタやC APIにもアクセスできるため、高レベルなプログラムから低レベルなネイティブ処理まで扱える設計になっています。
