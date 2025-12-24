package parser

import "core:fmt"
import "core:mem"
import "core:strconv"

import "../ast"
import "../lexer"
import "../logger"
import t "../token"


Parser :: struct {
	lexer:     ^lexer.Lexer,
	allocator: mem.Allocator,
	cur:       t.Token,
	peek:      t.Token,
	errors:    [dynamic]logger.CompilerError,
}

Precedence :: enum {
	LOWEST,
	ASSIGNMENT, // =
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
	case .ASSIGN:
		return .ASSIGNMENT
	case .EQ, .NOT_EQ:
		return .EQUALS
	case .LT, .GT, .LTE, .GTE:
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

PrefixParseFns :: proc(p: ^Parser) -> ast.Expr
InfixParseFns :: proc(p: ^Parser, expr: ast.Expr) -> ast.Expr

get_prefix_fn :: proc(tok_type: t.TokenType) -> PrefixParseFns {
	#partial switch (tok_type) {
	case .INT:
		return parse_int
	case .STRING:
		return parse_string
	case .IDENT:
		return parse_ident
	case .TRUE:
		return parse_bool
	case .FALSE:
		return parse_bool
	case .BANG:
		return parse_prefix_expr
	case .MINUS:
		return parse_prefix_expr
	case .L_PAREN:
		return parse_grouped_expr
	case .L_BRACKET:
		return parse_array
	case .AMPERSAND:
		return parse_pointer_expr
	case .STAR:
		return parse_deref_expr
	}
	return nil
}

get_infix_fn :: proc(tok_type: t.TokenType) -> InfixParseFns {
	#partial switch (tok_type) {
	case .PLUS:
		return parse_infix_expr
	case .MINUS:
		return parse_infix_expr
	case .STAR:
		return parse_infix_expr
	case .SLASH:
		return parse_infix_expr
	case .EQ:
		return parse_infix_expr
	case .NOT_EQ:
		return parse_infix_expr
	case .AND:
		return parse_infix_expr
	case .OR:
		return parse_infix_expr
	case .LT:
		return parse_infix_expr
	case .GT:
		return parse_infix_expr
	case .LTE:
		return parse_infix_expr
	case .GTE:
		return parse_infix_expr
	case .L_PAREN:
		return parse_call_expr
	case .L_BRACKET:
		return parse_index_expr
	case .ASSIGN:
		return parse_reassign_expr
	}
	return nil
}

init :: proc(p: ^Parser, file_contents: string, allocator: mem.Allocator) {
	l := new(lexer.Lexer, allocator)
	lexer.init(l, file_contents)
	p.lexer = l

	p.allocator = allocator
	p.errors = make([dynamic]logger.CompilerError, p.allocator)

	next_token(p)
	next_token(p)
}

parse_module :: proc(p: ^Parser) -> ^ast.Module {
	module := new(ast.Module, p.allocator)
	module.tok = p.cur
	module.stmts = make([dynamic]ast.Statement, p.allocator)
	for !cur_token_is(p, .EOF) {
		if stmt := parse_stmt(p); stmt != nil {
			append(&module.stmts, stmt)
		}
		next_token(p)
	}
	return module
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
		}
	}

	return parse_expr_stmt(p)
}

parse_deref_expr :: proc(p: ^Parser) -> ast.Expr {
	expr := new(ast.DerefExpr, p.allocator)
	expr.tok = p.cur
	next_token(p)
	expr.operand = parse_expr(p, .PREFIX)
	return expr
}

parse_pointer_expr :: proc(p: ^Parser) -> ast.Expr {
	expr := new(ast.PointerExpr, p.allocator)
	expr.tok = p.cur
	next_token(p)
	expr.operand = parse_expr(p, .PREFIX)
	return expr
}

parse_index_expr :: proc(p: ^Parser, left: ast.Expr) -> ast.Expr {
	expr := new(ast.IndexExpr, p.allocator)
	expr.tok = p.cur
	expr.left = left
	next_token(p)
	expr.index = parse_expr(p, .LOWEST)
	if !expect_peek(p, .R_BRACKET) {
		return nil
	}
	return expr
}

parse_break_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.BreakStatement, p.allocator)
	stmt.tok = p.cur
	if peek_token_is(p, .SEMI_COLON) {
		next_token(p)
	}
	return stmt
}

parse_loop_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.LoopStatement, p.allocator)
	stmt.tok = p.cur

	// loop { ... }
	if peek_token_is(p, .L_BRACE) {
		next_token(p)
		stmt.kind = .Infinite
		stmt.block = parse_block_stmt(p)
		return stmt
	}

	// loop when x < 10 { ... }
	if peek_token_is(p, .WHEN) {
		next_token(p)
		next_token(p)
		stmt.kind = .When
		stmt.wehn = parse_expr(p, .LOWEST)

		if !expect_peek(p, .L_BRACE) {
			return nil
		}
		stmt.block = parse_block_stmt(p)
		return stmt
	}

	// loop item in items { ... }
	// loop item, idx in items { ... }
	if peek_token_is(p, .IDENT) {
		next_token(p)
		stmt.kind = .Iterator
		stmt.item = parse_expr(p, .LOWEST)
		if peek_token_is(p, .COMMA) {
			next_token(p)
			next_token(p)
			stmt.idx = parse_expr(p, .LOWEST)
		}
		if !expect_peek(p, .IN) {
			return nil
		}
		next_token(p)
		stmt.items = parse_expr(p, .LOWEST)

		if !expect_peek(p, .L_BRACE) {
			return nil
		}
		stmt.block = parse_block_stmt(p)
		return stmt
	}

	error(p, "Unexpected loop syntax")
	return nil
}

parse_reassign_expr :: proc(p: ^Parser, left: ast.Expr) -> ast.Expr {
	reassign := new(ast.ReassignExpr, p.allocator)
	reassign.tok = p.cur
	reassign.target = left

	next_token(p)

	reassign.value = parse_expr(p, .LOWEST)
	if peek_token_is(p, .SEMI_COLON) {
		next_token(p)
	}

	return reassign
}

// Variations:
// foo: String;
// foo: String = "bar";
// foo := "bar";
parse_assign_stmt :: proc(p: ^Parser) -> ast.Statement {
	stmt := new(ast.AssignStatement, p.allocator)
	stmt.tok = p.cur
	stmt.name = parse_ident(p).(^ast.Identifier)

	if !expect_peek(p, .COLON) {
		return nil
	}

	if !peek_token_is(p, .ASSIGN) {
		next_token(p)
		type := parse_type_annotation(p)
		if type == nil {
			return nil
		}
		stmt.declared_type = type
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

	if peek_token_is(p, .SEMI_COLON) {
		next_token(p)
	}

	return stmt
}

parse_expr :: proc(p: ^Parser, precedence: Precedence) -> ast.Expr {
	prefix_fn := get_prefix_fn(p.cur.type)
	if prefix_fn == nil {
		error(p, "no prefix parse function found for %s", p.cur.type)
		return nil
	}
	left := prefix_fn(p)
	for !peek_token_is(p, .SEMI_COLON) &&
	    precedence < get_precedence(p.peek.type) {
		infix_fn := get_infix_fn(p.peek.type)
		if infix_fn == nil {
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

parse_array :: proc(p: ^Parser) -> ast.Expr {
	arr := new(ast.Array, p.allocator)
	arr.tok = p.cur
	arr.elements = make([dynamic]ast.Expr, p.allocator)

	next_token(p)
	append(&arr.elements, parse_expr(p, .LOWEST))
	for peek_token_is(p, .COMMA) {
		next_token(p) // comma
		next_token(p)
		append(&arr.elements, parse_expr(p, .LOWEST))
	}

	if !expect_peek(p, .R_BRACKET) {
		return nil
	}

	return arr
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
		type := parse_type_annotation(p)
		if type == nil {
			return nil
		}
		func.declared_return_type = type
	}

	if !expect_peek(p, .L_BRACE) {
		return nil
	}

	func.body = parse_block_stmt(p)
	return func
}

parse_type_annotation :: proc(p: ^Parser) -> ^ast.TypeAnnotation {
	if p.cur.type != .L_BRACKET &&
	   p.cur.type != .STAR &&
	   !t.is_type(p.cur.type) {
		error(p, "Expected a type, got %s", p.cur.literal)
		return nil
	}

	ta := new(ast.TypeAnnotation, p.allocator)
	ta.tok = p.cur

	#partial switch p.cur.type {
	case .STAR:
		next_token(p)
		if !t.is_type(p.cur.type) {
			error(p, "Expected a type, got %s", p.cur.literal)
			return nil
		}
		base_type := parse_type_annotation(p)
		ta.data = ast.PointerTypeAnnotation{base_type}
		return ta

	case .L_BRACKET:
		next_token(p)

		// slices
		if cur_token_is(p, .R_BRACKET) {
			next_token(p)
			elements_type := parse_type_annotation(p)
			ta.data = ast.SliceTypeAnnotation{elements_type}
			return ta
		}

		// arrays
		size_expr := parse_expr(p, .LOWEST)

		if !expect_peek(p, .R_BRACKET) {
			return nil
		}

		next_token(p)

		elements_type := parse_type_annotation(p)

		ta.data = ast.ArrayTypeAnnotation {
			elements_type = elements_type,
			size_expr     = size_expr,
		}

		return ta
	}
	return ta
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
		type := parse_type_annotation(p)
		if type == nil {
			return nil
		}
		arg.declared_type = type
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
			type := parse_type_annotation(p)
			if type == nil {
				return nil
			}
			arg.declared_type = type
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
