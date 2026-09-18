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

func (l *Lexer) peekAt(off int) rune {
	if l.pos+off >= len(l.src) {
		return 0
	}
	return l.src[l.pos+off]
}

func (l *Lexer) advance() rune {
	if l.pos >= len(l.src) {
		return 0
	}
	c := l.src[l.pos]
	l.pos++
	if c == '\n' {
		l.line++
		l.col = 1
	} else {
		l.col++
	}
	return c
}

func (l *Lexer) errorf(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	panic(fmt.Sprintf("%s:%d:%d: lex error: %s", l.filename, l.line, l.col, msg))
}

