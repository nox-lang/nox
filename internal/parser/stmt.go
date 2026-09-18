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

func (p *Parser) parseStmt() ast.Stmt {
	switch p.cur().Kind {
	case token.LET, token.PRIVATE:
		return p.parseLetStmt()
	case token.IF:
		return p.parseIfStmt()
	case token.FOR:
		return p.parseForStmt()
	case token.WHILE:
		return p.parseWhileStmt()
	case token.BREAK:
		bt := p.advance()
		bs := &ast.BreakStmt{Base: ast.NewBase(bt.Line, bt.Col)}
		if p.canStartExpr() {
			bs.Value = p.parseExpr()
		}
		return bs
	case token.NEXT:
		nt := p.advance()
		ns := &ast.NextStmt{Base: ast.NewBase(nt.Line, nt.Col)}
		if p.canStartExpr() {
			ns.Value = p.parseExpr()
		}
		return ns
	case token.YIELD:
		yt := p.advance()
		return &ast.YieldStmt{Base: ast.NewBase(yt.Line, yt.Col), Value: p.parseExpr()}
	case token.RETURN:
		rt := p.advance()
		rs := &ast.ReturnStmt{Base: ast.NewBase(rt.Line, rt.Col)}
		if p.canStartExpr() {
			rs.Value = p.parseExpr()
		}
		return rs
	case token.SWITCH:
		return p.parseSwitchStmt()
	case token.DEFER:
		dt := p.advance()
		return &ast.DeferStmt{Base: ast.NewBase(dt.Line, dt.Col), Body: p.parseBlock()}
	case token.TRY:
		return p.parseTryStmt()
	default:
		return p.parseExprOrAssignStmt()
	}
}

