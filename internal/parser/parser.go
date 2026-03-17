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
			p.errorf("'private' has been removed from Nox: a name starting with a lowercase letter is package-private, one starting with an uppercase letter is public")
		case p.at(token.STATIC):
			p.errorf("'static' is only valid on a class variable or method inside a class body")
		case p.at(token.CLASS):
			f.Classes = append(f.Classes, p.parseClassDecl())
		case p.at(token.TYPE):
			f.Types = append(f.Types, p.parseTypeDecl()...)
		case p.at(token.ASYNC), p.at(token.FUNC):
			f.Funcs = append(f.Funcs, p.parseFuncDecl())
		case p.at(token.LET):
			f.Globals = append(f.Globals, p.parseLetStmt())
		default:
			p.errorf("expected a top-level declaration (func, class, type, let, import, include)")
		}
	}
	return f
}

// parseTypeDecl parses `type Name = T`, `type Name T`, or the grouped form
// `type ( A = T  B T ... )`. Every form declares a transparent alias.
func (p *Parser) parseTypeDecl() []*ast.TypeDecl {
	p.expect(token.TYPE)
	one := func() *ast.TypeDecl {
		name := p.expect(token.IDENT)
		p.accept(token.ASSIGN)
		te := p.parseType()
		return &ast.TypeDecl{Base: ast.NewBase(name.Line, name.Col), Name: name.Literal, Type: te}
	}
	if p.accept(token.LPAREN) {
		var out []*ast.TypeDecl
		for !p.at(token.RPAREN) {
			out = append(out, one())
			p.accept(token.COMMA)
		}
		p.expect(token.RPAREN)
		return out
	}
	return []*ast.TypeDecl{one()}
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

func (p *Parser) parseInclude() []*ast.IncludeSpec {
	t := p.expect(token.INCLUDE)
	var specs []*ast.IncludeSpec
	p.expect(token.LPAREN)
	for !p.at(token.RPAREN) {
		st := p.expect(token.STRING)
		spec := &ast.IncludeSpec{Base: ast.NewBase(st.Line, st.Col), Header: st.Literal}
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

// ---------------- Types ----------------

func (p *Parser) parseType() *ast.TypeExpr {
	switch {
	case p.at(token.LBRACKET):
		lb := p.advance()
		if p.accept(token.RBRACKET) {
			return &ast.TypeExpr{Base: ast.NewBase(lb.Line, lb.Col), Name: "slice", Elem: p.parseType()}
		}
		n := p.expect(token.INT)
		p.expect(token.RBRACKET)
		return &ast.TypeExpr{Base: ast.NewBase(lb.Line, lb.Col), Name: "array", Len: parseIntLiteral(n.Literal), Elem: p.parseType()}
	case p.at(token.FUNC):
		ft := p.advance()
