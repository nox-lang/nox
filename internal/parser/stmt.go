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
	isPrivate := p.accept(token.PRIVATE)
	lt := p.expect(token.LET)
	name := p.expect(token.IDENT)
	ls := &ast.LetStmt{Base: ast.NewBase(lt.Line, lt.Col), Name: name.Literal, IsPrivate: isPrivate}
	if p.accept(token.COLON) {
		ls.Type = p.parseType()
	}
	if p.accept(token.ASSIGN) {
		ls.Value = p.parseExpr()
	}
	return ls
}

