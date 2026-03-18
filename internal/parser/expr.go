package parser

import (
	"nox/internal/ast"
	"nox/internal/token"
)

// Precedence (low -> high):
//   ||
//   &&
//   |
//   ^
//   &  (binary)
//   == !=
//   < > <= >=
//   + -
//   * / %
//   unary - ! & *
//   postfix . () [] ?
//   primary

func (p *Parser) parseExpr() ast.Expr { return p.parseOr() }

func (p *Parser) parseOr() ast.Expr {
	x := p.parseAnd()
	for p.at(token.OR) {
		t := p.advance()
		y := p.parseAnd()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: token.OR, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseAnd() ast.Expr {
	x := p.parseBitOr()
	for p.at(token.AND) {
		t := p.advance()
		y := p.parseBitOr()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: token.AND, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseBitOr() ast.Expr {
	x := p.parseBitXor()
	for p.at(token.PIPE) {
		t := p.advance()
		y := p.parseBitXor()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: token.PIPE, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseBitXor() ast.Expr {
	x := p.parseBitAnd()
	for p.at(token.CARET) {
		t := p.advance()
		y := p.parseBitAnd()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: token.CARET, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseBitAnd() ast.Expr {
	x := p.parseEquality()
	for p.at(token.AMP) {
		t := p.advance()
		y := p.parseEquality()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: token.AMP, X: x, Y: y}
	}
	return x
}

