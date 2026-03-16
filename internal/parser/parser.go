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

