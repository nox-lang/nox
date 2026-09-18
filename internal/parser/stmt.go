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

// canStartExpr reports whether the current token could begin an expression
// (used to decide whether `break`/`return` carries a value).
func (p *Parser) canStartExpr() bool {
	switch p.cur().Kind {
	case token.RBRACE, token.EOF, token.CASE, token.DEFAULT:
		return false
	default:
		return true
	}
}

func (p *Parser) parseIfStmt() ast.Stmt {
	it := p.expect(token.IF)
	p.expect(token.LPAREN)
	cond := p.parseExpr()
	p.expect(token.RPAREN)
	then := p.parseBlock()
	is := &ast.IfStmt{Base: ast.NewBase(it.Line, it.Col), Cond: cond, Then: then}
	if p.accept(token.ELSE) {
		if p.at(token.IF) {
			is.Else = p.parseIfStmt()
		} else {
			is.Else = p.parseBlock()
		}
	}
	return is
}

func (p *Parser) parseForStmt() ast.Stmt {
	ft := p.expect(token.FOR)
	p.expect(token.LPAREN)
	// Try to detect array form: (IDENT ("," IDENT)? "in" Expr)
	save := p.mark()
	if p.at(token.IDENT) {
		first := p.advance()
		if p.accept(token.COMMA) {
			if p.at(token.IDENT) {
				second := p.advance()
				if p.accept(token.IN) {
					arr := p.parseExpr()
					p.expect(token.RPAREN)
					body := p.parseBlock()
					return &ast.ForInStmt{Base: ast.NewBase(ft.Line, ft.Col), IndexName: first.Literal, ValueName: second.Literal, Array: arr, Body: body}
				}
			}
			p.reset(save)
		} else if p.accept(token.IN) {
			arr := p.parseExpr()
			p.expect(token.RPAREN)
			body := p.parseBlock()
			return &ast.ForInStmt{Base: ast.NewBase(ft.Line, ft.Col), ValueName: first.Literal, Array: arr, Body: body}
		} else {
			p.reset(save)
		}
	}
	// condition form
	var cond ast.Expr
	if !p.at(token.RPAREN) {
		cond = p.parseExpr()
	}
	p.expect(token.RPAREN)
	body := p.parseBlock()
	return &ast.ForCondStmt{Base: ast.NewBase(ft.Line, ft.Col), Cond: cond, Body: body}
}

func (p *Parser) parseWhileStmt() ast.Stmt {
	wt := p.expect(token.WHILE)
	p.expect(token.LPAREN)
	cond := p.parseExpr()
	p.expect(token.RPAREN)
	body := p.parseBlock()
	return &ast.WhileStmt{Base: ast.NewBase(wt.Line, wt.Col), Cond: cond, Body: body}
}

func (p *Parser) parseSwitchStmt() ast.Stmt {
	st := p.expect(token.SWITCH)
	p.expect(token.LPAREN)
	subj := p.parseExpr()
	p.expect(token.RPAREN)
	p.expect(token.LBRACE)
	ss := &ast.SwitchStmt{Base: ast.NewBase(st.Line, st.Col), Subject: subj}
	for !p.at(token.RBRACE) {
		if p.accept(token.CASE) {
			sc := &ast.SwitchCase{Base: ast.NewBase(p.cur().Line, p.cur().Col)}
			sc.Values = append(sc.Values, p.parseExpr())
			for p.accept(token.COMMA) {
				sc.Values = append(sc.Values, p.parseExpr())
			}
			sc.Body = p.parseBlock()
			ss.Cases = append(ss.Cases, sc)
		} else if p.accept(token.DEFAULT) {
			ss.Default = p.parseBlock()
		} else {
			p.errorf("expected 'case' or 'default' in switch body")
		}
	}
	p.expect(token.RBRACE)
	return ss
}

func (p *Parser) parseTryStmt() ast.Stmt {
	tt := p.expect(token.TRY)
	body := p.parseBlock()
	p.expect(token.CATCH)
	p.expect(token.LPAREN)
	name := p.expect(token.IDENT)
	p.expect(token.RPAREN)
	catchBody := p.parseBlock()
	return &ast.TryStmt{Base: ast.NewBase(tt.Line, tt.Col), Body: body, CatchVar: name.Literal, CatchBody: catchBody}
}

var assignOps = map[token.Kind]bool{
	token.ASSIGN: true, token.PLUSEQ: true, token.MINUSEQ: true, token.STAREQ: true, token.SLASHEQ: true,
}

