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

func (p *Parser) parseFile() *ast.File {
	f := &ast.File{Filename: p.filename}
	if p.at(token.PACKAGE) {
		p.advance()
		name := p.expect(token.IDENT)
		f.Package = name.Literal
	}
	for {
		switch {
		case p.at(token.IMPORT):
			f.Imports = append(f.Imports, p.parseImport()...)
		case p.at(token.INCLUDE):
			f.Includes = append(f.Includes, p.parseInclude()...)
		default:
			goto decls
		}
	}
decls:
	for !p.at(token.EOF) {
		switch {
		case p.at(token.PRIVATE):
			save := p.mark()
			p.advance()
			if p.at(token.CLASS) {
				p.reset(save)
				f.Classes = append(f.Classes, p.parseClassDecl())
			} else if p.at(token.ASYNC) || p.at(token.FUNC) {
				p.reset(save)
				f.Funcs = append(f.Funcs, p.parseFuncDecl())
			} else if p.at(token.LET) {
				p.reset(save)
				f.Globals = append(f.Globals, p.parseLetStmt())
			} else {
				p.errorf("expected class, func, or let after 'private'")
			}
		case p.at(token.CLASS):
			f.Classes = append(f.Classes, p.parseClassDecl())
		case p.at(token.ASYNC), p.at(token.FUNC):
			f.Funcs = append(f.Funcs, p.parseFuncDecl())
		case p.at(token.LET):
			f.Globals = append(f.Globals, p.parseLetStmt())
		default:
			p.errorf("expected a top-level declaration (func, class, let, import, include)")
		}
	}
	return f
}

func (p *Parser) parseImport() []*ast.ImportSpec {
	t := p.expect(token.IMPORT)
	var specs []*ast.ImportSpec
	p.expect(token.LPAREN)
	for !p.at(token.RPAREN) {
		st := p.expect(token.STRING)
		spec := &ast.ImportSpec{Base: ast.NewBase(st.Line, st.Col), Path: st.Literal}
		if p.accept(token.AS) {
			alias := p.expect(token.IDENT)
			spec.Alias = alias.Literal
		}
		specs = append(specs, spec)
		if !p.accept(token.COMMA) {
			break
		}
	}
	p.expect(token.RPAREN)
	_ = t
	return specs
}

