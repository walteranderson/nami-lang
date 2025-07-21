package nami

import "core:fmt"
import "core:mem"
import "core:strconv"
import "core:time"

PrefixParseFns :: proc(p: ^Parser) -> Expr
InfixParseFns :: proc(p: ^Parser, expr: Expr) -> Expr

Parser :: struct {
	lexer:      ^Lexer,
	allocator:  mem.Allocator,
	cur:        Token,
	peek:       Token,
	errors:     [dynamic]ParseError,
	prefix_fns: map[TokenType]PrefixParseFns,
	infix_fns:  map[TokenType]InfixParseFns,
}

ParseError :: struct {
	msg:  string,
	line: int,
	col:  int,
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

parser_init :: proc(p: ^Parser, file_contents: string, allocator: mem.Allocator) {
	lexer := new(Lexer, allocator)
	lexer_init(lexer, file_contents)
	p.lexer = lexer

	p.allocator = allocator
	p.errors = make([dynamic]ParseError, p.allocator)
	precedences_init(p.allocator)

	p.prefix_fns = make(map[TokenType]PrefixParseFns, p.allocator)
	p.prefix_fns[.INT] = parser_parse_int
	p.prefix_fns[.STRING] = parser_parse_string
	p.prefix_fns[.IDENT] = parser_parse_ident
	p.prefix_fns[.TRUE] = parser_parse_bool
	p.prefix_fns[.FALSE] = parser_parse_bool
	p.prefix_fns[.BANG] = parser_parse_prefix_expr
	p.prefix_fns[.MINUS] = parser_parse_prefix_expr
	p.prefix_fns[.L_PAREN] = parser_parse_grouped_expr

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
	start := time.now()
	log(.INFO, "Parsing program")

	program := new(Program, p.allocator)
	program.stmts = make([dynamic]Statement, p.allocator)
	for !parser_cur_token_is(p, .EOF) {
		if stmt := parser_parse_stmt(p); stmt != nil {
			append(&program.stmts, stmt)
		}
		parser_next_token(p)
	}

	log(.INFO, "Parsing complete: %v", time.diff(start, time.now()))
	return program
}

parser_parse_stmt :: proc(p: ^Parser) -> Statement {
	if parser_cur_token_is(p, .RETURN) {
		return parser_parse_return_stmt(p)
	}

	if parser_cur_token_is(p, .FUNC) {
		return parser_parse_func(p)
	}

	if parser_cur_token_is(p, .IF) {
		return parser_parse_if_stmt(p)
	}

	if parser_cur_token_is(p, .IDENT) {
		if parser_peek_token_is(p, .COLON) {
			return parser_parse_assign_stmt(p)
		} else if parser_peek_token_is(p, .ASSIGN) {
			return parser_parse_reassign_stmt(p)
		}
	}

	return parser_parse_expr_stmt(p)
}

parser_parse_reassign_stmt :: proc(p: ^Parser) -> Statement {
	stmt := new(ReassignStatement, p.allocator)
	stmt.tok = p.cur
	name := parser_parse_ident(p).(^Identifier)
	stmt.name = name

	if !parser_expect_peek(p, .ASSIGN) {
		return nil
	}
	parser_next_token(p)
	stmt.value = parser_parse_expr(p, .LOWEST)
	if parser_peek_token_is(p, .SEMI_COLON) {
		parser_next_token(p)
	}

	return stmt
}

parser_parse_assign_stmt :: proc(p: ^Parser) -> Statement {
	stmt := new(AssignStatement, p.allocator)
	stmt.tok = p.cur
	stmt.name = parser_parse_ident(p).(^Identifier)

	if !parser_expect_peek(p, .COLON) {
		return nil
	}

	if !parser_peek_token_is(p, .ASSIGN) {
		parser_next_token(p)
		stmt.declared_type = parser_parse_type_annotation(p)
	}

	if parser_peek_token_is(p, .ASSIGN) {
		parser_next_token(p)
		parser_next_token(p)
		stmt.value = parser_parse_expr(p, .LOWEST)
		if parser_peek_token_is(p, .SEMI_COLON) {
			parser_next_token(p)
		}
	} else {
		parser_next_token(p)
	}

	return stmt
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

parser_parse_if_stmt :: proc(p: ^Parser) -> Statement {
	parser_next_token(p)
	if_expr := new(IfStatement, p.allocator)
	if_expr.condition = parser_parse_expr(p, .LOWEST)
	if !parser_expect_peek(p, .L_BRACE) {
		return nil
	}
	if_expr.consequence = parser_parse_block_stmt(p)
	if parser_peek_token_is(p, .ELSE) {
		parser_next_token(p)
		if !parser_expect_peek(p, .L_BRACE) {
			return nil
		}
		if_expr.alternative = parser_parse_block_stmt(p)
	}

	return if_expr
}

parser_parse_grouped_expr :: proc(p: ^Parser) -> Expr {
	parser_next_token(p)
	expr := parser_parse_expr(p, .LOWEST)
	if !parser_expect_peek(p, .R_PAREN) {
		return nil
	}
	return expr
}

parser_parse_prefix_expr :: proc(p: ^Parser) -> Expr {
	expr := new(PrefixExpr, p.allocator)
	expr.tok = p.cur
	expr.op = p.cur.literal
	parser_next_token(p)
	expr.right = parser_parse_expr(p, .PREFIX)
	return expr
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

	ident, ok := left.(^Identifier)
	if !ok {
		parser_error(p, "expected call expression identifier, got %v", left)
		return nil
	}
	expr.func = ident

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
	if parser_peek_token_is(p, .SEMI_COLON) {
		parser_next_token(p)
	}
	return stmt
}

parser_parse_bool :: proc(p: ^Parser) -> Expr {
	b := new(Boolean, p.allocator)
	b.tok = p.cur
	b.value = parser_cur_token_is(p, .TRUE)
	return b
}

parser_parse_return_stmt :: proc(p: ^Parser) -> Statement {
	stmt := new(ReturnStatement, p.allocator)
	stmt.tok = p.cur

	parser_next_token(p)

	// empty return with no expr
	if parser_cur_token_is(p, .SEMI_COLON) {
		return stmt
	}

	stmt.value = parser_parse_expr(p, .LOWEST)
	if !parser_cur_token_is(p, .SEMI_COLON) {
		parser_next_token(p)
	}

	return stmt
}

parser_parse_func :: proc(p: ^Parser) -> Statement {
	func := new(FunctionStatement, p.allocator)
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

	if !parser_peek_token_is(p, .L_BRACE) {
		parser_next_token(p)
		func.declared_return_type = parser_parse_type_annotation(p)
	}

	if !parser_expect_peek(p, .L_BRACE) {
		return nil
	}

	func.body = parser_parse_block_stmt(p)
	return func
}

parser_parse_type_annotation :: proc(p: ^Parser) -> ^TypeAnnotation {
	t := new(TypeAnnotation, p.allocator)
	t.tok = p.cur
	t.name = p.cur.literal
	return t
}

parser_parse_block_stmt :: proc(p: ^Parser) -> ^BlockStatement {
	block := new(BlockStatement, p.allocator)
	block.tok = p.cur
	block.stmts = make([dynamic]Statement, p.allocator)

	parser_next_token(p)

	for !parser_cur_token_is(p, .R_BRACE) && !parser_cur_token_is(p, .EOF) {
		stmt := parser_parse_stmt(p)
		if stmt != nil {
			append(&block.stmts, stmt)
		}
		parser_next_token(p)
	}
	return block
}

parser_parse_func_args :: proc(p: ^Parser) -> [dynamic]^FunctionArg {
	args := make([dynamic]^FunctionArg, p.allocator)
	if parser_peek_token_is(p, .R_PAREN) {
		parser_next_token(p)
		return args
	}

	parser_next_token(p)
	arg := new(FunctionArg, p.allocator)
	arg.ident = parser_parse_ident(p).(^Identifier)

	if parser_peek_token_is(p, .COLON) {
		parser_next_token(p) // colon
		parser_next_token(p)
		arg.declared_type = parser_parse_type_annotation(p)
	}

	append(&args, arg)

	for parser_peek_token_is(p, .COMMA) {
		parser_next_token(p) // comma
		parser_next_token(p)
		arg := new(FunctionArg, p.allocator)
		arg.ident = parser_parse_ident(p).(^Identifier)

		if parser_peek_token_is(p, .COLON) {
			parser_next_token(p) // colon
			parser_next_token(p)
			arg.declared_type = parser_parse_type_annotation(p)
		}

		append(&args, arg)
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
	msg := fmt.tprintf(ft, ..args)
	append(&p.errors, ParseError{msg = msg, line = p.cur.line, col = p.cur.col})
}

parser_expect_peek :: proc(p: ^Parser, type: TokenType) -> bool {
	if p.peek.type != type {
		parser_error(p, "expected token %s, got %s", type, p.peek.type)
		return false
	}
	parser_next_token(p)
	return true
}
