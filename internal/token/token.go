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

	// Keywords
	PACKAGE
	IMPORT
	INCLUDE
	AS
	LET
	FUNC
	RETURN
	IF
	ELSE
	FOR
	WHILE
	IN
	BREAK
	NEXT
	YIELD
	SWITCH
	CASE
	DEFAULT
	CLASS
	PRIVATE // removed from the language; still lexed so that a clear error can be reported
	STATIC
	TYPE
	THIS
	TRUE
	FALSE
	NULL
	ASYNC
	AWAIT
	PARALLEL
	TRY
	CATCH
	DEFER

