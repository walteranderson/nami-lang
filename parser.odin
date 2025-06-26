package main

import "core:fmt"
import "core:mem"

Parser :: struct {
	lexer:     ^Lexer,
	allocator: mem.Allocator,
	cur:       Token,
	peek:      Token,
	errors:    [dynamic]string,
}

parser_init :: proc(p: ^Parser, lexer: ^Lexer, allocator: mem.Allocator) {
	p.lexer = lexer
	p.allocator = allocator
	p.errors = make([dynamic]string, p.allocator)

	parser_next_token(p)
	parser_next_token(p)
}

parser_parse_program :: proc(p: ^Parser) -> ^Program {
	program := new(Program, p.allocator)
	program.stmts = make([dynamic]Statement, p.allocator)
	for !parser_cur_token_is(p, .EOF) {
		stmt := parser_parse_stmt(p)
		if stmt != nil {
			append(&program.stmts, stmt)
		}
		parser_next_token(p)
	}
	return program
}

parser_parse_stmt :: proc(p: ^Parser) -> Statement {
	return nil
}

parser_next_token :: proc(p: ^Parser) {
	p.cur = p.peek
	p.peek = lexer_next_token(p.lexer)
}

parser_cur_token_is :: proc(p: ^Parser, type: TokenType) -> bool {
	return p.cur.type == type
}

parser_peek_token_is :: proc(p: ^Parser, type: TokenType) -> bool {
	return p.peek.type == type
}

parser_error :: proc(p: ^Parser, ft: string, args: ..any) {
	append(&p.errors, fmt.tprintf(ft, ..args))
}

parser_expect_peek :: proc(p: ^Parser, type: TokenType) -> bool {
	if p.peek.type != type {
		parser_error(p, "expected token %s, got %s", type, p.peek.type)
		return false
	}
	parser_next_token(p)
	return true
}
