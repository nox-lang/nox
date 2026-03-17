package parser

import (
	"nox/internal/ast"
	"nox/internal/token"
)

func (p *Parser) parseBlock() *ast.BlockStmt {
	lb := p.expect(token.LBRACE)
	b := &ast.BlockStmt{Base: ast.NewBase(lb.Line, lb.Col)}
	for !p.at(token.RBRACE) {
