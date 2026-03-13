// Package lexer converts Nox source text into a stream of tokens.
package lexer

import (
	"fmt"
	"strings"

	"nox/internal/token"
)

type Lexer struct {
	src      []rune
	pos      int
	line     int
	col      int
	filename string
}

func New(src, filename string) *Lexer {
	return &Lexer{src: []rune(src), pos: 0, line: 1, col: 1, filename: filename}
}

func (l *Lexer) peekCh() rune {
	if l.pos >= len(l.src) {
		return 0
	}
	return l.src[l.pos]
}

