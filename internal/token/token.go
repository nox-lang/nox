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

	// Punctuation
	LPAREN   // (
	RPAREN   // )
	LBRACE   // {
	RBRACE   // }
	LBRACKET // [
	RBRACKET // ]
	COMMA    // ,
	COLON    // :
	SEMI     // ; (implicit, not usually written)
	DOT      // .
	DCOLON   // ::
	ELLIPSIS // ...
	QUESTION // ?

	// Operators
	ASSIGN   // =
	PLUS     // +
	MINUS    // -
	STAR     // *
	SLASH    // /
	PERCENT  // %
	AMP      // &
	PIPE     // |
	CARET    // ^
	NOT      // !
	LT       // <
	GT       // >
	LE       // <=
	GE       // >=
	EQ       // ==
	NE       // !=
	AND      // &&
	OR       // ||
	PLUSEQ   // +=
	MINUSEQ  // -=
	STAREQ   // *=
	SLASHEQ  // /=
	PLUSPLUS // ++
	MINUSMINUS
)

var keywords = map[string]Kind{
	"package":  PACKAGE,
	"import":   IMPORT,
	"include":  INCLUDE,
	"as":       AS,
	"let":      LET,
	"func":     FUNC,
	"return":   RETURN,
	"if":       IF,
	"else":     ELSE,
	"for":      FOR,
	"while":    WHILE,
	"in":       IN,
	"break":    BREAK,
	"next":     NEXT,
	"yield":    YIELD,
	"switch":   SWITCH,
	"case":     CASE,
	"default":  DEFAULT,
	"class":    CLASS,
	"private":  PRIVATE,
	"static":   STATIC,
	"type":     TYPE,
	"this":     THIS,
	"true":     TRUE,
	"false":    FALSE,
	"null":     NULL,
	"async":    ASYNC,
	"await":    AWAIT,
	"parallel": PARALLEL,
	"Parallel": PARALLEL,
	"try":      TRY,
	"catch":    CATCH,
	"defer":    DEFER,
}

func Lookup(ident string) Kind {
	if k, ok := keywords[ident]; ok {
		return k
	}
	return IDENT
}

type Token struct {
	Kind    Kind
	Literal string
	Line    int
	Col     int
}

func (k Kind) String() string {
	names := map[Kind]string{
		EOF: "EOF", ILLEGAL: "ILLEGAL", IDENT: "IDENT", INT: "INT", FLOAT: "FLOAT", STRING: "STRING",
		PACKAGE: "package", IMPORT: "import", INCLUDE: "include", AS: "as", LET: "let", FUNC: "func",
