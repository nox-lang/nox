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
