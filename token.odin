package main

import "core:mem"
import "core:unicode/utf8"

TokenType :: enum {
	ILLEGAL,
	EOF,
	L_PAREN,
	R_PAREN,
	L_BRACE,
	R_BRACE,
	STRING,
	INT,
	IDENT,
	FUNC,
	SEMI_COLON,
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
		return TokenType.FUNC
	case:
		return TokenType.IDENT
	}
}
