package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"

Lexer :: struct {
	input:    string,
	pos:      int,
	read_pos: int,
	ch:       rune,
}

lexer_init :: proc(l: ^Lexer, input: string) {
	l.input = input
	lexer_read_char(l)
}

lexer_next_token :: proc(l: ^Lexer) -> Token {
	lexer_skip_whitespace(l)
	tok: Token

	switch (l.ch) {
	case '(':
		tok.type = .L_PAREN
		tok.literal = l.input[l.pos:l.pos + 1]
	case ')':
		tok.type = .R_PAREN
		tok.literal = l.input[l.pos:l.pos + 1]
	case '{':
		tok.type = .L_BRACE
		tok.literal = l.input[l.pos:l.pos + 1]
	case '}':
		tok.type = .R_BRACE
		tok.literal = l.input[l.pos:l.pos + 1]
	case '[':
		tok.type = .L_BRACKET
		tok.literal = l.input[l.pos:l.pos + 1]
	case ']':
		tok.type = .R_BRACKET
		tok.literal = l.input[l.pos:l.pos + 1]
	case ':':
		tok.type = .COLON
		tok.literal = l.input[l.pos:l.pos + 1]
	case ';':
		tok.type = .SEMI_COLON
		tok.literal = l.input[l.pos:l.pos + 1]
	case '<':
		tok.type = .LT
		tok.literal = l.input[l.pos:l.pos + 1]
	case '>':
		tok.type = .GT
		tok.literal = l.input[l.pos:l.pos + 1]
	case '+':
		tok.type = .PLUS
		tok.literal = l.input[l.pos:l.pos + 1]
	case '-':
		tok.type = .MINUS
		tok.literal = l.input[l.pos:l.pos + 1]
	case '*':
		tok.type = .STAR
		tok.literal = l.input[l.pos:l.pos + 1]
	case '/':
		if lexer_peek_char(l) == '/' {
			lexer_skip_line(l)
			return lexer_next_token(l)
		} else {
			tok.type = .SLASH
			tok.literal = l.input[l.pos:l.pos + 1]
		}
	case ',':
		tok.type = .COMMA
		tok.literal = l.input[l.pos:l.pos + 1]
	case '=':
		if lexer_peek_char(l) == '=' {
			tok.type = .EQ
			pos := l.pos
			lexer_read_char(l)
			tok.literal = l.input[pos:l.pos + 1]
		} else {
			tok.type = .ASSIGN
			tok.literal = l.input[l.pos:l.pos + 1]
		}
	case '!':
		if lexer_peek_char(l) == '=' {
			tok.type = .NOT_EQ
			pos := l.pos
			lexer_read_char(l)
			tok.literal = l.input[pos:l.pos + 1]
		} else {
			tok.type = .BANG
			tok.literal = l.input[l.pos:l.pos + 1]
		}
	case 0:
		tok.type = TokenType.EOF
		tok.literal = ""
	case:
		if is_string(l.ch) {
			tok.type = TokenType.STRING
			tok.literal = lexer_read_string(l)
		} else if unicode.is_letter(l.ch) {
			tok.literal = lexer_read_ident(l)
			tok.type = ident_lookup(tok.literal)
			return tok
		} else if unicode.is_digit(l.ch) {
			tok.type = TokenType.INT
			tok.literal = lexer_read_number(l)
			return tok
		} else {
			tok.type = .ILLEGAL
			tok.literal = l.input[l.pos:l.pos + 1]
		}
	}
	lexer_read_char(l)
	return tok
}

lexer_read_char :: proc(l: ^Lexer) {
	if l.read_pos >= len(l.input) {
		l.ch = 0
	} else {
		reader: strings.Reader
		strings.reader_init(&reader, l.input[l.read_pos:])
		rr, size, err := strings.reader_read_rune(&reader)
		if err != nil {
			fmt.eprintln("error reading rune")
			l.ch = 0
			return
		}
		l.ch = rr
		l.pos = l.read_pos
		l.read_pos += size
	}
}

lexer_peek_char :: proc(l: ^Lexer) -> rune {
	if l.read_pos >= len(l.input) {
		return 0
	}
	r, _ := utf8.decode_rune_in_string(l.input[l.read_pos:])
	return r
}

lexer_skip_line :: proc(l: ^Lexer) {
	for l.ch != '\n' {
		lexer_read_char(l)
	}
}

lexer_skip_whitespace :: proc(l: ^Lexer) {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r' || l.ch == '\n' {
		lexer_read_char(l)
	}
}

lexer_read_ident :: proc(l: ^Lexer) -> string {
	start := l.pos
	for unicode.is_letter(l.ch) {
		lexer_read_char(l)
	}
	return l.input[start:l.pos]
}

lexer_read_string :: proc(l: ^Lexer) -> string {
	start := l.pos + 1
	for {
		lexer_read_char(l)
		if is_string(l.ch) || l.ch == 0 {
			break
		}
	}
	return l.input[start:l.pos]
}

lexer_read_number :: proc(l: ^Lexer) -> string {
	start := l.pos
	for unicode.is_digit(l.ch) {
		lexer_read_char(l)
	}
	return l.input[start:l.pos]
}

is_string :: proc(ch: rune) -> bool {
	return ch == '"' || ch == '\'' || ch == '`'
}
