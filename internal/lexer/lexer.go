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

// Tokenize returns all tokens in the source, terminated by an EOF token.
func Tokenize(src, filename string) []token.Token {
	l := New(src, filename)
	var toks []token.Token
	for {
		t := l.Next()
		toks = append(toks, t)
		if t.Kind == token.EOF {
			break
		}
	}
	return toks
}

func isDigit(c rune) bool { return c >= '0' && c <= '9' }
func isAlpha(c rune) bool {
	return c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c > 127
}
func isAlnum(c rune) bool { return isAlpha(c) || isDigit(c) }

func (l *Lexer) skipWhitespaceAndComments() {
	for {
		c := l.peekCh()
		if c == ' ' || c == '\t' || c == '\r' || c == '\n' {
			l.advance()
			continue
		}
		if c == '/' && l.peekAt(1) == '/' {
			for l.peekCh() != '\n' && l.peekCh() != 0 {
				l.advance()
			}
			continue
		}
		if c == '/' && l.peekAt(1) == '*' {
			l.advance()
			l.advance()
			for {
				if l.peekCh() == 0 {
					l.errorf("unterminated block comment")
				}
				if l.peekCh() == '*' && l.peekAt(1) == '/' {
					l.advance()
					l.advance()
					break
				}
				l.advance()
			}
			continue
		}
		break
	}
}

func (l *Lexer) Next() token.Token {
	l.skipWhitespaceAndComments()
	line, col := l.line, l.col
	c := l.peekCh()
	if c == 0 {
		return token.Token{Kind: token.EOF, Line: line, Col: col}
	}

	if isDigit(c) {
		return l.lexNumber(line, col)
	}
	if isAlpha(c) {
		return l.lexIdent(line, col)
	}
	if c == '"' {
		return l.lexString(line, col)
	}

	// operators / punctuation
	switch c {
	case '(':
		l.advance()
		return token.Token{Kind: token.LPAREN, Literal: "(", Line: line, Col: col}
	case ')':
		l.advance()
		return token.Token{Kind: token.RPAREN, Literal: ")", Line: line, Col: col}
	case '{':
		l.advance()
		return token.Token{Kind: token.LBRACE, Literal: "{", Line: line, Col: col}
	case '}':
		l.advance()
		return token.Token{Kind: token.RBRACE, Literal: "}", Line: line, Col: col}
	case '[':
		l.advance()
		return token.Token{Kind: token.LBRACKET, Literal: "[", Line: line, Col: col}
	case ']':
		l.advance()
		return token.Token{Kind: token.RBRACKET, Literal: "]", Line: line, Col: col}
	case ',':
		l.advance()
		return token.Token{Kind: token.COMMA, Literal: ",", Line: line, Col: col}
	case '?':
		l.advance()
		return token.Token{Kind: token.QUESTION, Literal: "?", Line: line, Col: col}
	case ':':
		l.advance()
		if l.peekCh() == ':' {
			l.advance()
			return token.Token{Kind: token.DCOLON, Literal: "::", Line: line, Col: col}
		}
		return token.Token{Kind: token.COLON, Literal: ":", Line: line, Col: col}
	case '.':
		if l.peekAt(1) == '.' && l.peekAt(2) == '.' {
			l.advance()
			l.advance()
			l.advance()
			return token.Token{Kind: token.ELLIPSIS, Literal: "...", Line: line, Col: col}
		}
		l.advance()
		return token.Token{Kind: token.DOT, Literal: ".", Line: line, Col: col}
	case '+':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.PLUSEQ, Literal: "+=", Line: line, Col: col}
		}
		if l.peekCh() == '+' {
			l.advance()
			return token.Token{Kind: token.PLUSPLUS, Literal: "++", Line: line, Col: col}
		}
		return token.Token{Kind: token.PLUS, Literal: "+", Line: line, Col: col}
	case '-':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.MINUSEQ, Literal: "-=", Line: line, Col: col}
		}
		if l.peekCh() == '-' {
			l.advance()
			return token.Token{Kind: token.MINUSMINUS, Literal: "--", Line: line, Col: col}
		}
		return token.Token{Kind: token.MINUS, Literal: "-", Line: line, Col: col}
	case '*':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.STAREQ, Literal: "*=", Line: line, Col: col}
		}
		return token.Token{Kind: token.STAR, Literal: "*", Line: line, Col: col}
	case '/':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.SLASHEQ, Literal: "/=", Line: line, Col: col}
		}
		return token.Token{Kind: token.SLASH, Literal: "/", Line: line, Col: col}
	case '%':
		l.advance()
		return token.Token{Kind: token.PERCENT, Literal: "%", Line: line, Col: col}
	case '&':
		l.advance()
		if l.peekCh() == '&' {
			l.advance()
			return token.Token{Kind: token.AND, Literal: "&&", Line: line, Col: col}
		}
		return token.Token{Kind: token.AMP, Literal: "&", Line: line, Col: col}
	case '|':
		l.advance()
		if l.peekCh() == '|' {
			l.advance()
			return token.Token{Kind: token.OR, Literal: "||", Line: line, Col: col}
		}
		return token.Token{Kind: token.PIPE, Literal: "|", Line: line, Col: col}
	case '^':
		l.advance()
		return token.Token{Kind: token.CARET, Literal: "^", Line: line, Col: col}
	case '!':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.NE, Literal: "!=", Line: line, Col: col}
		}
		return token.Token{Kind: token.NOT, Literal: "!", Line: line, Col: col}
	case '<':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.LE, Literal: "<=", Line: line, Col: col}
		}
		return token.Token{Kind: token.LT, Literal: "<", Line: line, Col: col}
	case '>':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.GE, Literal: ">=", Line: line, Col: col}
		}
		return token.Token{Kind: token.GT, Literal: ">", Line: line, Col: col}
	case '=':
		l.advance()
		if l.peekCh() == '=' {
			l.advance()
			return token.Token{Kind: token.EQ, Literal: "==", Line: line, Col: col}
		}
		return token.Token{Kind: token.ASSIGN, Literal: "=", Line: line, Col: col}
	}

	l.errorf("unexpected character %q", c)
	return token.Token{}
}

