package parser

import (
	"nox/internal/ast"
	"nox/internal/token"
)

func (p *Parser) parseBlock() *ast.BlockStmt {
	lb := p.expect(token.LBRACE)
	b := &ast.BlockStmt{Base: ast.NewBase(lb.Line, lb.Col)}
	for !p.at(token.RBRACE) {
		b.Stmts = append(b.Stmts, p.parseStmt())
	}
	p.expect(token.RBRACE)
	return b
}

func (p *Parser) parseLetStmt() *ast.LetStmt {
	lt := p.expect(token.LET)
	name := p.expect(token.IDENT)
	ls := &ast.LetStmt{Base: ast.NewBase(lt.Line, lt.Col), Name: name.Literal}
