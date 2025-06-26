package main

import "core:fmt"
import "core:mem"
import "core:strconv"

PrefixParseFns :: proc(p: ^Parser) -> Expr
InfixParseFns :: proc(p: ^Parser, expr: Expr) -> Expr

Parser :: struct {
	lexer:      ^Lexer,
	allocator:  mem.Allocator,
	cur:        Token,
	peek:       Token,
	errors:     [dynamic]string,
	prefix_fns: map[TokenType]PrefixParseFns,
	infix_fns:  map[TokenType]InfixParseFns,
}

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
	precedences[EQ] = EQUALS
	precedences[NOT_EQ] = EQUALS
	precedences[LT] = LESSGREATER
	precedences[GT] = LESSGREATER
	precedences[PLUS] = SUM
	precedences[MINUS] = SUM
	precedences[SLASH] = PRODUCT
	precedences[STAR] = PRODUCT
	precedences[L_PAREN] = CALL
	precedences[L_BRACKET] = INDEX
}

get_precedence :: proc(tt: TokenType) -> Precedence {
	if val, ok := precedences[tt]; ok {
		return val
	}
	return .LOWEST
}

parser_init :: proc(p: ^Parser, lexer: ^Lexer, allocator: mem.Allocator) {
	p.lexer = lexer
	p.allocator = allocator
	p.errors = make([dynamic]string, p.allocator)
	precedences_init(p.allocator)

	p.prefix_fns = make(map[TokenType]PrefixParseFns, p.allocator)
	p.prefix_fns[.INT] = parser_parse_int
	p.prefix_fns[.STRING] = parser_parse_string
	p.prefix_fns[.IDENT] = parser_parse_ident
	p.prefix_fns[.FUNC] = parser_parse_func

	p.infix_fns = make(map[TokenType]InfixParseFns, p.allocator)
	p.infix_fns[.PLUS] = parser_parse_infix_expr
	p.infix_fns[.MINUS] = parser_parse_infix_expr
	p.infix_fns[.STAR] = parser_parse_infix_expr
	p.infix_fns[.SLASH] = parser_parse_infix_expr
	p.infix_fns[.EQ] = parser_parse_infix_expr
	p.infix_fns[.NOT_EQ] = parser_parse_infix_expr
	p.infix_fns[.LT] = parser_parse_infix_expr
	p.infix_fns[.GT] = parser_parse_infix_expr
	p.infix_fns[.L_PAREN] = parser_parse_call_expr

	parser_next_token(p)
	parser_next_token(p)
}

parser_parse_program :: proc(p: ^Parser) -> ^Program {
	program := new(Program, p.allocator)
	program.stmts = make([dynamic]Statement, p.allocator)
	for !parser_cur_token_is(p, .EOF) {
		fmt.println("parsing statement")
		if stmt := parser_parse_stmt(p); stmt != nil {
			append(&program.stmts, stmt)
		}
		parser_next_token(p)
	}
	return program
}

parser_parse_stmt :: proc(p: ^Parser) -> Statement {
	#partial switch p.cur.type {
	case .RETURN:
		return parser_parse_return_stmt(p)
	case:
		return parser_parse_expr_stmt(p)
	}
}

parser_parse_expr :: proc(p: ^Parser, precedence: Precedence) -> Expr {
	prefix_fn, ok := p.prefix_fns[p.cur.type]
	if !ok {
		parser_error(p, "no prefix parse function found for %s", p.cur.type)
		return nil
	}
	left := prefix_fn(p)
	for !parser_peek_token_is(p, .SEMI_COLON) && precedence < get_precedence(p.peek.type) {
		infix_fn, ok := p.infix_fns[p.peek.type]
		if !ok {
			return left
		}
		parser_next_token(p)
		left = infix_fn(p, left)
	}
	return left
}

parser_parse_infix_expr :: proc(p: ^Parser, left: Expr) -> Expr {
	infix := new(InfixExpr, p.allocator)
	infix.tok = p.cur
	infix.left = left
	infix.op = p.cur.literal

	precedence := get_precedence(p.cur.type)
	parser_next_token(p)
	infix.right = parser_parse_expr(p, precedence)

	return infix
}

parser_parse_call_expr :: proc(p: ^Parser, left: Expr) -> Expr {
	expr := new(CallExpr, p.allocator)
	expr.tok = p.cur
	expr.func = left
	expr.args = parser_parse_expr_list(p, .R_PAREN)
	return expr
}

parser_parse_expr_list :: proc(p: ^Parser, end: TokenType) -> [dynamic]Expr {
	list := make([dynamic]Expr, p.allocator)
	if parser_peek_token_is(p, end) {
		parser_next_token(p)
		return list
	}
	parser_next_token(p)
	append(&list, parser_parse_expr(p, .LOWEST))
	for parser_peek_token_is(p, .COMMA) {
		parser_next_token(p) // comma
		parser_next_token(p)
		append(&list, parser_parse_expr(p, .LOWEST))
	}
	if !parser_expect_peek(p, .R_PAREN) {
		return nil
	}
	return list
}

parser_parse_expr_stmt :: proc(p: ^Parser) -> Statement {
	stmt := new(ExprStatement, p.allocator)
	stmt.tok = p.cur
	stmt.value = parser_parse_expr(p, .LOWEST)
	return stmt
}

parser_parse_return_stmt :: proc(p: ^Parser) -> Statement {
	stmt := new(ReturnStatement, p.allocator)
	stmt.tok = p.cur

	parser_next_token(p)
	stmt.value = parser_parse_expr(p, .LOWEST)

	if !parser_cur_token_is(p, .SEMI_COLON) {
		parser_next_token(p)
	}

	return stmt
}

parser_parse_func :: proc(p: ^Parser) -> Expr {
	func := new(Function, p.allocator)
	func.tok = p.cur
	if !parser_expect_peek(p, .IDENT) {
		return nil
	}
	name := parser_parse_ident(p).(^Identifier)
	func.name = name
	if !parser_expect_peek(p, .L_PAREN) {
		return nil
	}
	func.args = parser_parse_func_args(p)
	if !parser_expect_peek(p, .L_BRACE) {
		return nil
	}
	func.body = parser_parse_block_stmt(p)
	return func
}

parser_parse_block_stmt :: proc(p: ^Parser) -> ^BlockStatement {
	block := new(BlockStatement, p.allocator)
	block.tok = p.cur
	block.stmts = make([dynamic]Statement, p.allocator)
	parser_next_token(p)
	for !parser_peek_token_is(p, .R_BRACE) && !parser_peek_token_is(p, .EOF) {
		stmt := parser_parse_stmt(p)
		if stmt != nil {
			append(&block.stmts, stmt)
		}
		parser_next_token(p)
	}
	return block
}

parser_parse_func_args :: proc(p: ^Parser) -> [dynamic]^Identifier {
	args := make([dynamic]^Identifier, p.allocator)
	if parser_peek_token_is(p, .R_PAREN) {
		parser_next_token(p)
		return args
	}
	parser_next_token(p)
	append(&args, parser_parse_ident(p).(^Identifier))
	for parser_peek_token_is(p, .COMMA) {
		parser_next_token(p) // comma
		parser_next_token(p)
		append(&args, parser_parse_ident(p).(^Identifier))
	}
	if !parser_expect_peek(p, .R_PAREN) {
		return nil
	}
	return args
}

parser_parse_ident :: proc(p: ^Parser) -> Expr {
	ident := new(Identifier, p.allocator)
	ident.tok = p.cur
	ident.value = p.cur.literal
	return ident
}

parser_parse_int :: proc(p: ^Parser) -> Expr {
	num := new(IntLiteral, p.allocator)
	num.tok = p.cur
	num.value = strconv.atoi(p.cur.literal)
	return num
}

parser_parse_string :: proc(p: ^Parser) -> Expr {
	str := new(StringLiteral, p.allocator)
	str.tok = p.cur
	str.value = p.cur.literal
	return str
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
