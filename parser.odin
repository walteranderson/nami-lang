package main


import "core:fmt"
import "core:mem"

Parser :: struct {
	lexer:      ^Lexer,
	program:    ^Program,
	allocator:  mem.Allocator,
	cur:        Token,
	peek:       Token,
	errors:     [dynamic]string,
	has_main:   bool,
	prefix_fns: map[TokenType]PrefixParseFns,
	infix_fns:  map[TokenType]InfixParseFns,
}

PrefixParseFns :: proc(p: ^Parser) -> Expr
InfixParseFns :: proc(p: ^Parser, expr: Expr) -> Expr

Precedence :: enum {
	LOWEST,
	EQUALS, // ==
	LESSGREATER, // < or >
	SUM, // +
	PRODUCT, // *
	PREFIX, // -X or !X
	CALL, // myFunc(X)
	INDEX, // array[index]
}

precedences: map[TokenType]Precedence

precedences_init :: proc(allocator: mem.Allocator) {
	using Precedence, TokenType
	precedences = make(map[TokenType]Precedence, allocator)
	precedences[EQ] = EQUALS
}

get_precedence :: proc(tt: TokenType) -> Precedence {
	if val, ok := precedences[tt]; ok {
		return val
	}
	return .LOWEST
}

parser_init :: proc(p: ^Parser, l: ^Lexer, allocator: mem.Allocator) {
	p.lexer = l
	p.allocator = allocator
	p.errors = make([dynamic]string, allocator)
	precedences_init(allocator)

	p.prefix_fns = make(map[TokenType]PrefixParseFns, allocator)
	p.prefix_fns[.STRING] = parser_parse_string

	parser_next_token(p)
	parser_next_token(p)
}

parser_parse :: proc(p: ^Parser) -> ^Program {
	program := ast_new(Program, p.cur, p.allocator)

	stmts := make([dynamic]Stmt, p.allocator)
	for !parser_cur_token_is(p, .EOF) {
		stmt := parser_parse_stmt(p)
		if stmt != nil {
			append(&stmts, stmt)
		}
		parser_next_token(p)
	}
	program.stmts = stmts

	if !p.has_main {
		parser_error(p, "Missing main function")
	}

	return program
}

parser_parse_stmt :: proc(p: ^Parser) -> Stmt {
	if p.cur.type == .FUNC {
		return parser_parse_func(p)
	}
	return parser_parse_expr_stmt(p)
}

parser_parse_expr_stmt :: proc(p: ^Parser) -> ^ExprStmt {
	stmt := ast_new(ExprStmt, p.cur, p.allocator)
	stmt.expr = parser_parse_expr(p)
	return stmt
}

parser_parse_expr :: proc(p: ^Parser) -> Expr {
	// if prefixFn, ok := p.prefix_fns[p.cur.type]; !ok {
	// }

	#partial switch (p.cur.type) {
	case .STRING:
		return parser_parse_string(p)
	case .IDENT:
		if parser_peek_token_is(p, .L_PAREN) {
			return parser_parse_call_expr(p)
		}
	}
	parser_error(p, "unexpected token: %v", p.cur)
	return nil
}

parser_parse_string :: proc(p: ^Parser) -> Expr {
	str := ast_new(StringLiteral, p.cur, p.allocator)
	str.value = p.cur.literal
	return str
}

parser_parse_func :: proc(p: ^Parser) -> ^Function {
	def := ast_new(Function, p.cur, p.allocator)
	if !parser_expect_peek(p, .IDENT) {
		return nil
	}
	def.name = p.cur.literal
	if def.name == "main" {
		if p.has_main {
			parser_error(p, "More than one main function declared. Only one main function allowed")
		} else {
			p.has_main = true
		}
	}

	if !parser_expect_peek(p, .L_PAREN) {
		return nil
	}

	// TODO: add support for args

	if !parser_expect_peek(p, .R_PAREN) {
		return nil
	}

	if !parser_expect_peek(p, .L_BRACE) {
		return nil
	}

	def.body = parser_parse_block_stmt(p)

	return def
}

parser_parse_call_expr :: proc(p: ^Parser) -> ^CallExpr {
	expr := ast_new(CallExpr, p.cur, p.allocator)
	expr.callee = p.cur.literal
	if !parser_expect_peek(p, .L_PAREN) {
		return nil
	}
	parser_next_token(p)

	args := make([dynamic]Expr, p.allocator)
	for !parser_cur_token_is(p, .R_PAREN) && !parser_cur_token_is(p, .EOF) {
		arg := parser_parse_expr(p)
		if arg != nil {
			append(&args, arg)
		}
		parser_next_token(p)
	}
	expr.args = args
	if !parser_expect_peek(p, .SEMI_COLON) {
		return nil
	}

	return expr
}

parser_parse_block_stmt :: proc(p: ^Parser) -> ^BlockStmt {
	block := ast_new(BlockStmt, p.cur, p.allocator)
	parser_next_token(p)

	stmts := make([dynamic]Stmt, p.allocator)
	for !parser_cur_token_is(p, .R_BRACE) && !parser_cur_token_is(p, .EOF) {
		stmt := parser_parse_stmt(p)
		if stmt != nil {
			append(&stmts, stmt)
		}
		parser_next_token(p)
	}
	block.stmts = stmts

	return block
}

parser_expect_peek :: proc(p: ^Parser, type: TokenType) -> bool {
	if p.peek.type != type {
		parser_error(p, "expected token %s, got %s", type, p.peek.type)
		return false
	}
	parser_next_token(p)
	return true
}

parser_next_token :: proc(p: ^Parser) {
	p.cur = p.peek
	p.peek = lexer_next_token(p.lexer)
}

parser_peek_token_is :: proc(p: ^Parser, type: TokenType) -> bool {
	return p.peek.type == type
}

parser_cur_token_is :: proc(p: ^Parser, type: TokenType) -> bool {
	return p.cur.type == type
}

parser_error :: proc(p: ^Parser, ft: string, args: ..any) {
	append(&p.errors, fmt.tprintf(ft, ..args))
}
