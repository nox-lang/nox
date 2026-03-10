// Package token defines the lexical tokens of the Nox language.
package token

type Kind int

const (
	EOF Kind = iota
	ILLEGAL

	IDENT
	INT
	FLOAT
	STRING

