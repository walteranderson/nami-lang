package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"

Lexer :: struct {
	input:     string,
	pos:       int,
	read_pos:  int,
	ch:        rune,
	allocator: mem.Allocator,
}

lexer_init :: proc(l: ^Lexer, allocator: mem.Allocator, input: string) {
	l.input = input
	l.allocator = allocator
	lexer_read_char(l)
}

lexer_next_token :: proc(l: ^Lexer) -> Token {
	lexer_skip_whitespace(l)
	tok: Token

	switch (l.ch) {
	case '(':
		tok = token_new(.L_PAREN, l.ch, l.allocator)
	case ')':
		tok = token_new(.R_PAREN, l.ch, l.allocator)
	case '{':
		tok = token_new(.L_BRACE, l.ch, l.allocator)
	case '}':
		tok = token_new(.R_BRACE, l.ch, l.allocator)
	case '[':
		tok = token_new(.L_BRACKET, l.ch, l.allocator)
	case ']':
		tok = token_new(.R_BRACKET, l.ch, l.allocator)
	case ';':
		tok = token_new(.SEMI_COLON, l.ch, l.allocator)
	case '<':
		tok = token_new(.LT, l.ch, l.allocator)
	case '>':
		tok = token_new(.GT, l.ch, l.allocator)
	case '+':
		tok = token_new(.PLUS, l.ch, l.allocator)
	case '-':
		tok = token_new(.MINUS, l.ch, l.allocator)
	case '*':
		tok = token_new(.STAR, l.ch, l.allocator)
	case '/':
		tok = token_new(.SLASH, l.ch, l.allocator)
	case ',':
		tok = token_new(.COMMA, l.ch, l.allocator)
	case '=':
		if lexer_peek_char(l) == '=' {
			tok.type = .EQ
			ch := l.ch
			lexer_read_char(l)
			tok.literal = utf8.runes_to_string([]rune{ch, l.ch}, l.allocator)
		}
	case '!':
		if lexer_peek_char(l) == '=' {
			tok.type = .NOT_EQ
			ch := l.ch
			lexer_read_char(l)
			tok.literal = utf8.runes_to_string([]rune{ch, l.ch}, l.allocator)
		} else {
			tok = token_new(.BANG, l.ch, l.allocator)
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
			tok = token_new(.ILLEGAL, l.ch, l.allocator)
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
