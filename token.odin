package main

import "core:mem"
import "core:unicode/utf8"

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
	//
	EQ,
	NOT_EQ,
	LT,
	GT,
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
	//
	TYPE_STRING,
	TYPE_INT,
	TYPE_BOOL,
	TYPE_VOID,
}

Token :: struct {
	type:    TokenType,
	literal: string,
}

token_new :: proc(type: TokenType, ch: rune, allocator: mem.Allocator) -> Token {
	return Token{type = type, literal = utf8.runes_to_string([]rune{ch}, allocator)}
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
	case "string":
		return .TYPE_STRING
	case "int":
		return .TYPE_INT
	case "bool":
		return .TYPE_BOOL
	case "void":
		return .TYPE_VOID
	case:
		return .IDENT
	}
}
