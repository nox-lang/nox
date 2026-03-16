// Package parser implements a recursive-descent parser producing a Nox AST.
package parser

import (
	"fmt"

	"nox/internal/ast"
	"nox/internal/lexer"
	"nox/internal/token"
)

type Parser struct {
	toks     []token.Token
	pos      int
	filename string
}

func Parse(src, filename string) (file *ast.File, err error) {
	defer func() {
		if r := recover(); r != nil {
			switch v := r.(type) {
			case parseError:
				err = fmt.Errorf("%s", string(v))
			case string:
				err = fmt.Errorf("%s", v)
			case error:
				err = v
			default:
				panic(r)
			}
		}
	}()
	toks := lexer.Tokenize(src, filename)
	p := &Parser{toks: toks, pos: 0, filename: filename}
	return p.parseFile(), nil
}

type parseError string

func (p *Parser) errorf(format string, args ...interface{}) {
	t := p.cur()
	msg := fmt.Sprintf(format, args...)
	panic(parseError(fmt.Sprintf("%s:%d:%d: parse error: %s (got %q)", p.filename, t.Line, t.Col, msg, tokDesc(t))))
}

func tokDesc(t token.Token) string {
	if t.Literal != "" {
		return t.Literal
	}
	return t.Kind.String()
}

func (p *Parser) cur() token.Token { return p.toks[p.pos] }
func (p *Parser) peek(n int) token.Token {
	i := p.pos + n
	if i >= len(p.toks) {
		return p.toks[len(p.toks)-1]
	}
	return p.toks[i]
}
func (p *Parser) at(k token.Kind) bool { return p.cur().Kind == k }
func (p *Parser) advance() token.Token {
	t := p.cur()
	if p.pos < len(p.toks)-1 {
		p.pos++
	}
	return t
}
func (p *Parser) expect(k token.Kind) token.Token {
	if !p.at(k) {
		p.errorf("expected %s", k.String())
	}
	return p.advance()
}
func (p *Parser) accept(k token.Kind) bool {
	if p.at(k) {
		p.advance()
		return true
	}
	return false
}

func (p *Parser) mark() int   { return p.pos }
func (p *Parser) reset(m int) { p.pos = m }

// ---------------- File level ----------------

