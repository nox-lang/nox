// Package runtime embeds the Nox C runtime prelude (runtime.c) so the
// compiler can prepend it to every generated program without needing to
// ship the .c file separately.
package runtime

import _ "embed"

//go:embed c/runtime.c
var Prelude string
