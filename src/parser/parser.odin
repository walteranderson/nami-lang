package parser

import "core:fmt"
import "core:mem"
import "core:strconv"

import "../ast"
import "../lexer"
import "../logger"
import t "../token"

PrefixParseFns :: proc(p: ^Parser) -> ast.Expr
InfixParseFns :: proc(p: ^Parser, expr: ast.Expr) -> ast.Expr

Parser :: struct {
	lexer:      ^lexer.Lexer,
	allocator:  mem.Allocator,
	cur:        t.Token,
	peek:       t.Token,
	errors:     [dynamic]logger.CompilerError,
	prefix_fns: map[t.TokenType]PrefixParseFns,
	infix_fns:  map[t.TokenType]InfixParseFns,
}

Precedence :: enum {
	LOWEST,
	OR,
	AND,
	EQUALS, // ==
	LESSGREATER, // < or >
	SUM, // +
	PRODUCT, // *
	PREFIX, // -X or !X
	CALL, // myFunc(X)
	INDEX, // array[index]
}

get_precedence :: proc(tt: t.TokenType) -> Precedence {
	#partial switch tt {
	case .OR:
		return .OR
	case .AND:
		return .AND
	case .EQ, .NOT_EQ:
		return .EQUALS
	case .LT, .GT:
		return .LESSGREATER
	case .PLUS, .MINUS:
		return .SUM
	case .SLASH, .STAR:
		return .PRODUCT
	case .L_PAREN:
		return .CALL
	case .L_BRACKET:
		return .INDEX
	case:
		return Precedence.LOWEST
	}
}

init :: proc(p: ^Parser, file_contents: string, allocator: mem.Allocator) {
	l := new(lexer.Lexer, allocator)
	lexer.init(l, file_contents)
	p.lexer = l

	p.allocator = allocator
	p.errors = make([dynamic]logger.CompilerError, p.allocator)

	p.prefix_fns = make(map[t.TokenType]PrefixParseFns, p.allocator)
	p.prefix_fns[.INT] = parse_int
	p.prefix_fns[.STRING] = parse_string
	p.prefix_fns[.IDENT] = parse_ident
	p.prefix_fns[.TRUE] = parse_bool
	p.prefix_fns[.FALSE] = parse_bool
	p.prefix_fns[.BANG] = parse_prefix_expr
	p.prefix_fns[.MINUS] = parse_prefix_expr
	p.prefix_fns[.L_PAREN] = parse_grouped_expr

	p.infix_fns = make(map[t.TokenType]InfixParseFns, p.allocator)
	p.infix_fns[.PLUS] = parse_infix_expr
	p.infix_fns[.MINUS] = parse_infix_expr
	p.infix_fns[.STAR] = parse_infix_expr
	p.infix_fns[.SLASH] = parse_infix_expr
	p.infix_fns[.EQ] = parse_infix_expr
	p.infix_fns[.NOT_EQ] = parse_infix_expr
	p.infix_fns[.AND] = parse_infix_expr
	p.infix_fns[.OR] = parse_infix_expr
	p.infix_fns[.LT] = parse_infix_expr
	p.infix_fns[.GT] = parse_infix_expr
	p.infix_fns[.L_PAREN] = parse_call_expr

	next_token(p)
	next_token(p)
}

parse_program :: proc(p: ^Parser) -> ^ast.Program {
	program := new(ast.Program, p.allocator)
	program.tok = p.cur
	program.stmts = make([dynamic]ast.Statement, p.allocator)
	for !cur_token_is(p, .EOF) {
		if stmt := parse_stmt(p); stmt != nil {
			append(&program.stmts, stmt)
		}
		next_token(p)
	}
	return program
}

parse_stmt :: proc(p: ^Parser) -> ast.Statement {
	#partial switch p.cur.type {
	case .RETURN:
		return parse_return_stmt(p)
	case .FUNC:
		return parse_func(p)
	case .IF:
		return parse_if_stmt(p)
	case .LOOP:
		return parse_loop_stmt(p)
	case .BREAK:
		return parse_break_stmt(p)
	case .IDENT:
		if peek_token_is(p, .COLON) {
			return parse_assign_stmt(p)
		} else if peek_token_is(p, .ASSIGN) {
			return parse_reassign_stmt(p)
		}
	}

	return parse_expr_stmt(p)
}

parse_break_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.BreakStatement, p.allocator)
	stmt.tok = p.cur
	return stmt
}

parse_loop_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.LoopStatement, p.allocator)
	stmt.tok = p.cur
	if !expect_peek(p, .L_BRACE) {
		return nil
	}
	stmt.block = parse_block_stmt(p)
	return stmt
}

parse_reassign_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.ReassignStatement, p.allocator)
	stmt.tok = p.cur
	name := parse_ident(p).(^ast.Identifier)
	stmt.name = name

	if !expect_peek(p, .ASSIGN) {
		return nil
	}
	next_token(p)
	stmt.value = parse_expr(p, .LOWEST)
	if peek_token_is(p, .SEMI_COLON) {
		next_token(p)
	}

	return stmt
}

parse_assign_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.AssignStatement, p.allocator)
	stmt.tok = p.cur
	stmt.name = parse_ident(p).(^ast.Identifier)

	if !expect_peek(p, .COLON) {
		return nil
	}

	if !peek_token_is(p, .ASSIGN) {
		next_token(p)
		if !expect_type(p) {
			return nil
		}
		stmt.declared_type = parse_type_annotation(p)
	}

	if peek_token_is(p, .ASSIGN) {
		next_token(p)
		next_token(p)
		stmt.value = parse_expr(p, .LOWEST)
		if peek_token_is(p, .SEMI_COLON) {
			next_token(p)
		}
	} else {
		next_token(p)
	}

	return stmt
}

parse_expr :: proc(p: ^Parser, precedence: Precedence) -> ast.Expr {
	prefix_fn, ok := p.prefix_fns[p.cur.type]
	if !ok {
		error(p, "no prefix parse function found for %s", p.cur.type)
		return nil
	}
	left := prefix_fn(p)
	for !peek_token_is(p, .SEMI_COLON) && precedence < get_precedence(p.peek.type) {
		infix_fn, ok := p.infix_fns[p.peek.type]
		if !ok {
			return left
		}
		next_token(p)
		left = infix_fn(p, left)
	}
	return left
}

parse_if_stmt :: proc(p: ^Parser) -> ast.Statement {
	next_token(p)
	if_expr := new(ast.IfStatement, p.allocator)
	if_expr.condition = parse_expr(p, .LOWEST)
	if !expect_peek(p, .L_BRACE) {
		return nil
	}
	if_expr.consequence = parse_block_stmt(p)
	if peek_token_is(p, .ELSE) {
		next_token(p)
		if !expect_peek(p, .L_BRACE) {
			return nil
		}
		if_expr.alternative = parse_block_stmt(p)
	}

	return if_expr
}

parse_grouped_expr :: proc(p: ^Parser) -> ast.Expr {
	next_token(p)
	expr := parse_expr(p, .LOWEST)
	if !expect_peek(p, .R_PAREN) {
		return nil
	}
	return expr
}

parse_prefix_expr :: proc(p: ^Parser) -> ast.Expr {
	expr := new(ast.PrefixExpr, p.allocator)
	expr.tok = p.cur
	expr.op = p.cur.literal
	next_token(p)
	expr.right = parse_expr(p, .PREFIX)
	return expr
}

parse_infix_expr :: proc(p: ^Parser, left: ast.Expr) -> ast.Expr {
	infix := new(ast.InfixExpr, p.allocator)
	infix.tok = p.cur
	infix.left = left
	infix.op = p.cur.literal

	precedence := get_precedence(p.cur.type)
	next_token(p)
	infix.right = parse_expr(p, precedence)

	return infix
}

parse_call_expr :: proc(p: ^Parser, left: ast.Expr) -> ast.Expr {
	expr := new(ast.CallExpr, p.allocator)
	expr.tok = p.cur

	ident, ok := left.(^ast.Identifier)
	if !ok {
		error(p, "expected call expression identifier, got %v", left)
		return nil
	}
	expr.func = ident

	expr.args = parse_expr_list(p, .R_PAREN)
	return expr
}

parse_expr_list :: proc(p: ^Parser, end: t.TokenType) -> [dynamic]ast.Expr {
	list := make([dynamic]ast.Expr, p.allocator)
	if peek_token_is(p, end) {
		next_token(p)
		return list
	}
	next_token(p)
	append(&list, parse_expr(p, .LOWEST))
	for peek_token_is(p, .COMMA) {
		next_token(p) // comma
		next_token(p)
		append(&list, parse_expr(p, .LOWEST))
	}
	if !expect_peek(p, .R_PAREN) {
		return nil
	}
	return list
}

parse_expr_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.ExprStatement, p.allocator)
	stmt.tok = p.cur
	stmt.value = parse_expr(p, .LOWEST)
	if peek_token_is(p, .SEMI_COLON) {
		next_token(p)
	}
	return stmt
}

parse_bool :: proc(p: ^Parser) -> ast.Expr {
	b := new(ast.Boolean, p.allocator)
	b.tok = p.cur
	b.value = cur_token_is(p, .TRUE)
	return b
}

parse_return_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.ReturnStatement, p.allocator)
	stmt.tok = p.cur

	next_token(p)

	// empty return with no expr
	if cur_token_is(p, .SEMI_COLON) {
		return stmt
	}

	stmt.value = parse_expr(p, .LOWEST)
	if !cur_token_is(p, .SEMI_COLON) {
		next_token(p)
	}

	return stmt
}

parse_func :: proc(p: ^Parser) -> ast.Statement {
	func := new(ast.FunctionStatement, p.allocator)
	func.tok = p.cur
	if !expect_peek(p, .IDENT) {
		return nil
	}
	name := parse_ident(p).(^ast.Identifier)
	func.name = name
	if !expect_peek(p, .L_PAREN) {
		return nil
	}
	func.args = parse_func_args(p)

	if !peek_token_is(p, .L_BRACE) {
		next_token(p)
		if !expect_type(p) {
			return nil
		}
		func.declared_return_type = parse_type_annotation(p)
	}

	if !expect_peek(p, .L_BRACE) {
		return nil
	}

	func.body = parse_block_stmt(p)
	return func
}

parse_type_annotation :: proc(p: ^Parser) -> ^ast.TypeAnnotation {
	t := new(ast.TypeAnnotation, p.allocator)
	t.tok = p.cur
	t.name = p.cur.literal
	return t
}

parse_block_stmt :: proc(p: ^Parser) -> ^ast.BlockStatement {
	block := new(ast.BlockStatement, p.allocator)
	block.tok = p.cur
	block.stmts = make([dynamic]ast.Statement, p.allocator)

	next_token(p)

	for !cur_token_is(p, .R_BRACE) && !cur_token_is(p, .EOF) {
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&block.stmts, stmt)
		}
		next_token(p)
	}
	return block
}

parse_func_args :: proc(p: ^Parser) -> [dynamic]^ast.FunctionArg {
	args := make([dynamic]^ast.FunctionArg, p.allocator)
	if peek_token_is(p, .R_PAREN) {
		next_token(p)
		return args
	}

	next_token(p)
	arg := new(ast.FunctionArg, p.allocator)
	arg.ident = parse_ident(p).(^ast.Identifier)

	if peek_token_is(p, .COLON) {
		next_token(p) // colon
		next_token(p)
		if !expect_type(p) {
			return nil
		}
		arg.declared_type = parse_type_annotation(p)
	}

	append(&args, arg)

	for peek_token_is(p, .COMMA) {
		next_token(p) // comma
		next_token(p)
		arg := new(ast.FunctionArg, p.allocator)
		arg.ident = parse_ident(p).(^ast.Identifier)

		if peek_token_is(p, .COLON) {
			next_token(p) // colon
			next_token(p)
			if !expect_type(p) {
				return nil
			}
			arg.declared_type = parse_type_annotation(p)
		}

		append(&args, arg)
	}

	if !expect_peek(p, .R_PAREN) {
		return nil
	}
	return args
}

parse_ident :: proc(p: ^Parser) -> ast.Expr {
	ident := new(ast.Identifier, p.allocator)
	ident.tok = p.cur
	ident.value = p.cur.literal
	return ident
}

parse_int :: proc(p: ^Parser) -> ast.Expr {
	num := new(ast.IntLiteral, p.allocator)
	num.tok = p.cur
	num.value = strconv.atoi(p.cur.literal)
	return num
}

parse_string :: proc(p: ^Parser) -> ast.Expr {
	str := new(ast.StringLiteral, p.allocator)
	str.tok = p.cur
	str.value = p.cur.literal
	return str
}

next_token :: proc(p: ^Parser) {
	p.cur = p.peek
	p.peek = lexer.next_token(p.lexer)
}

cur_token_is :: proc(p: ^Parser, type: t.TokenType) -> bool {
	return p.cur.type == type
}

peek_token_is :: proc(p: ^Parser, type: t.TokenType) -> bool {
	return p.peek.type == type
}

error :: proc(p: ^Parser, ft: string, args: ..any) {
	// TODO: I feel like this shouldn't use the temporary allocator
	msg := fmt.tprintf(ft, ..args)
	err := logger.CompilerError {
		msg  = msg,
		line = p.cur.line,
		col  = p.cur.col,
	}
	append(&p.errors, err)
}

expect_peek :: proc(p: ^Parser, type: t.TokenType) -> bool {
	if p.peek.type != type {
		error(p, "expected token %s, got %s", type, p.peek.type)
		return false
	}
	next_token(p)
	return true
}

expect_type :: proc(p: ^Parser) -> bool {
	if !t.is_type(p.cur.type) {
		error(p, "Expected a type, got %s", p.cur.literal)
		return false
	}
	return true
}
