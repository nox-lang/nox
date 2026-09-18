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

func (p *Parser) parseEquality() ast.Expr {
	x := p.parseRelational()
	for p.at(token.EQ) || p.at(token.NE) {
		t := p.advance()
		y := p.parseRelational()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: t.Kind, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseRelational() ast.Expr {
	x := p.parseAdditive()
	for p.at(token.LT) || p.at(token.GT) || p.at(token.LE) || p.at(token.GE) {
		t := p.advance()
		y := p.parseAdditive()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: t.Kind, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseAdditive() ast.Expr {
	x := p.parseMultiplicative()
	for p.at(token.PLUS) || p.at(token.MINUS) {
		t := p.advance()
		y := p.parseMultiplicative()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: t.Kind, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseMultiplicative() ast.Expr {
	x := p.parseUnary()
	for p.at(token.STAR) || p.at(token.SLASH) || p.at(token.PERCENT) {
		t := p.advance()
		y := p.parseUnary()
		x = &ast.BinaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: t.Kind, X: x, Y: y}
	}
	return x
}

func (p *Parser) parseUnary() ast.Expr {
	switch p.cur().Kind {
	case token.MINUS, token.NOT, token.AMP, token.STAR:
		t := p.advance()
		x := p.parseUnary()
		return &ast.UnaryExpr{Base: ast.NewBase(t.Line, t.Col), Op: t.Kind, X: x}
	case token.AWAIT:
		t := p.advance()
		x := p.parseUnary()
		return &ast.AwaitExpr{Base: ast.NewBase(t.Line, t.Col), X: x}
	}
	return p.parsePostfix()
}

func (p *Parser) parsePostfix() ast.Expr {
	x := p.parsePrimary()
	for {
		switch p.cur().Kind {
		case token.DOT:
			p.advance()
			name := p.expect(token.IDENT)
			x = &ast.MemberExpr{Base: ast.NewBase(name.Line, name.Col), X: x, Name: name.Literal}
		case token.LPAREN:
			lp := p.advance()
			var args []ast.Expr
			for !p.at(token.RPAREN) {
				args = append(args, p.parseExpr())
				if !p.accept(token.COMMA) {
					break
				}
			}
			p.expect(token.RPAREN)
			x = &ast.CallExpr{Base: ast.NewBase(lp.Line, lp.Col), Callee: x, Args: args}
		case token.LBRACKET:
			lb := p.advance()
			idx := p.parseExpr()
			p.expect(token.RBRACKET)
			x = &ast.IndexExpr{Base: ast.NewBase(lb.Line, lb.Col), X: x, Index: idx}
		case token.QUESTION:
			q := p.advance()
			x = &ast.PropagateExpr{Base: ast.NewBase(q.Line, q.Col), X: x}
		default:
			return x
		}
	}
}

func (p *Parser) parsePrimary() ast.Expr {
	t := p.cur()
	switch t.Kind {
	case token.INT:
		p.advance()
		return &ast.IntLit{Base: ast.NewBase(t.Line, t.Col), Value: parseIntLiteral(t.Literal)}
	case token.FLOAT:
		p.advance()
		return &ast.FloatLit{Base: ast.NewBase(t.Line, t.Col), Value: parseFloatLiteral(t.Literal)}
	case token.STRING:
		p.advance()
		return &ast.StringLit{Base: ast.NewBase(t.Line, t.Col), Value: t.Literal}
	case token.TRUE:
		p.advance()
		return &ast.BoolLit{Base: ast.NewBase(t.Line, t.Col), Value: true}
	case token.FALSE:
		p.advance()
		return &ast.BoolLit{Base: ast.NewBase(t.Line, t.Col), Value: false}
	case token.NULL:
		p.advance()
		return &ast.NullLit{Base: ast.NewBase(t.Line, t.Col)}
	case token.THIS:
		p.advance()
		return &ast.ThisExpr{Base: ast.NewBase(t.Line, t.Col)}
	case token.PARALLEL:
		return p.parseParallelExpr()
	case token.IF:
		return p.parseIfStmt().(ast.Expr)
	case token.FOR:
		return p.parseForStmt().(ast.Expr)
	case token.WHILE:
		return p.parseWhileStmt().(ast.Expr)
	case token.SWITCH:
		return p.parseSwitchStmt().(ast.Expr)
	case token.LBRACKET:
		return p.parseArrayLit()
	case token.IDENT:
		return p.parseIdentOrQualOrFuncLit()
	case token.LPAREN:
		if fl, ok := p.tryParseFuncLit(); ok {
			return fl
		}
		p.advance()
		x := p.parseExpr()
		p.expect(token.RPAREN)
		return x
	}
	p.errorf("expected an expression")
	return nil
}

func (p *Parser) parseIdentOrQualOrFuncLit() ast.Expr {
	first := p.expect(token.IDENT)
	if p.at(token.DCOLON) {
		parts := []string{first.Literal}
		for p.accept(token.DCOLON) {
			id := p.expect(token.IDENT)
			parts = append(parts, id.Literal)
		}
		return &ast.QualIdent{Base: ast.NewBase(first.Line, first.Col), Parts: parts}
	}
	return &ast.Ident{Base: ast.NewBase(first.Line, first.Col), Name: first.Literal}
}

// tryParseFuncLit attempts to parse `(params) { body }` at the current
// position (which is a LPAREN). On failure it rewinds and returns false.
func (p *Parser) tryParseFuncLit() (ast.Expr, bool) {
	save := p.mark()
	lp := p.cur()
	ok := func() bool {
		defer func() { recover() }() // treat any parse error as "not a func lit"
		p.advance()                  // (
		for !p.at(token.RPAREN) {
			if !p.at(token.IDENT) {
				return false
			}
			p.advance()
			if p.at(token.COLON) {
				p.advance()
				p.parseType()
			}
			if p.at(token.ELLIPSIS) {
				p.advance()
			}
			if p.at(token.ASSIGN) {
				p.advance()
				p.parseExpr()
			}
			if !p.at(token.COMMA) {
				break
			}
			p.advance()
		}
		if !p.at(token.RPAREN) {
			return false
		}
		p.advance() // )
		if p.at(token.COLON) {
			p.advance()
			p.parseType()
		}
		return p.at(token.LBRACE)
	}()
	if !ok {
		p.reset(save)
		return nil, false
	}
	p.reset(save)
	// Re-parse for real (constructing AST nodes this time).
	p.advance() // (
	var params []*ast.Param
	for !p.at(token.RPAREN) {
		nt := p.expect(token.IDENT)
		param := &ast.Param{Base: ast.NewBase(nt.Line, nt.Col), Name: nt.Literal}
		if p.accept(token.COLON) {
			param.Type = p.parseType()
		}
		if p.accept(token.ELLIPSIS) {
			param.Variadic = true
		}
		if p.accept(token.ASSIGN) {
			param.Default = p.parseExpr()
		}
		params = append(params, param)
		if !p.accept(token.COMMA) {
			break
		}
	}
	p.expect(token.RPAREN)
	fl := &ast.FuncLit{Base: ast.NewBase(lp.Line, lp.Col), Params: params}
	if p.accept(token.COLON) {
		fl.ReturnType = p.parseType()
	}
	fl.Body = p.parseBlock()
	return fl, true
}

func (p *Parser) parseArrayLit() ast.Expr {
	lb := p.expect(token.LBRACKET)
	al := &ast.ArrayLit{Base: ast.NewBase(lb.Line, lb.Col)}
	for !p.at(token.RBRACKET) {
		al.Elems = append(al.Elems, p.parseExpr())
		if !p.accept(token.COMMA) {
			break
		}
	}
	p.expect(token.RBRACKET)
	return al
}

// parseParallelExpr parses `Parallel { call() call() ... }`.
func (p *Parser) parseParallelExpr() ast.Expr {
	pt := p.expect(token.PARALLEL)
	p.expect(token.LBRACE)
	pe := &ast.ParallelExpr{Base: ast.NewBase(pt.Line, pt.Col)}
	for !p.at(token.RBRACE) {
		pe.Calls = append(pe.Calls, p.parseExpr())
	}
	p.expect(token.RBRACE)
	return pe
}
