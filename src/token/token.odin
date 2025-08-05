package token

TokenType :: enum {
	ILLEGAL,
	EOF,
	//
	STRING,
	INT,
	IDENT,
	ASSIGN,
	//
	L_PAREN,
	R_PAREN,
	L_BRACE,
	R_BRACE,
	L_BRACKET,
	R_BRACKET,
	COLON,
	SEMI_COLON,
	COMMA,
	AMPERSAND,
	PIPE,
	//
	EQ,
	NOT_EQ,
	LT,
	GT,
	AND,
	OR,
	PLUS,
	MINUS,
	BANG,
	STAR,
	SLASH,
	//
	FUNC,
	TRUE,
	FALSE,
	RETURN,
	IF,
	ELSE,
	LOOP,
	BREAK,
	//
	TYPE_STRING,
	TYPE_INT,
	TYPE_BOOL,
	TYPE_VOID,
}

Token :: struct {
	type:    TokenType,
	literal: string,
	line:    int,
	col:     int,
}

is_type :: proc(t: TokenType) -> bool {
	#partial switch t {
	case .TYPE_INT, .TYPE_BOOL, .TYPE_VOID, .TYPE_STRING:
		return true
	case:
		return false
	}
}

ident_lookup :: proc(ident: string) -> TokenType {
	switch ident {
	case "fn":
		return .FUNC
	case "true":
		return .TRUE
	case "false":
		return .FALSE
	case "return":
		return .RETURN
	case "if":
		return .IF
	case "else":
		return .ELSE
	case "loop":
		return .LOOP
	case "break":
		return .BREAK
	case "String":
		return .TYPE_STRING
	case "Int":
		return .TYPE_INT
	case "Bool":
		return .TYPE_BOOL
	case "Void":
		return .TYPE_VOID
	case:
		return .IDENT
	}
}
