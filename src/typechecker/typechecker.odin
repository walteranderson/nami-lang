package typechecker

import "core:fmt"
import "core:mem"
import "core:strings"

import "../ast"
import "../logger"
import "../token"

TypeChecker :: struct {
	errors:    [dynamic]logger.CompilerError,
	allocator: mem.Allocator,
	program:   ^ast.Program,
	symbols:   [dynamic]map[string]^ast.TypeInfo,
	has_main:  bool,
}

init :: proc(tc: ^TypeChecker, p: ^ast.Program, allocator: mem.Allocator) {
	tc.program = p
	tc.allocator = allocator
	tc.errors = make([dynamic]logger.CompilerError, allocator)
	symbols_push_scope(tc)
}

check_program :: proc(tc: ^TypeChecker) {
	check_funcs(tc)
	if len(tc.errors) > 0 {
		return
	}

	for stmt in tc.program.stmts {
		check_stmt(tc, stmt)
	}

	if !tc.has_main {
		error(tc, tc.program.tok, "Missing main function")
	}
}

check_funcs :: proc(tc: ^TypeChecker) {
	for stmt in tc.program.stmts {
		if fn, ok := stmt.(^ast.FunctionStatement); ok {
			check_func_signature(tc, fn)
		}
	}
}

check_func_signature :: proc(tc: ^TypeChecker, fn: ^ast.FunctionStatement) {
	_, found := lookup_symbol(tc, fn.name.value)
	if found {
		error(tc, fn.name.tok, "Function already declared: %s", fn.name.value)
		return
	}

	for arg in fn.args {
		arg.resolved_type = make_typeinfo(tc, check_type_annotation(tc, arg.declared_type))
	}

	declared_return_type := check_type_annotation(tc, fn.declared_return_type)
	if declared_return_type == .Invalid {
		// TODO: this will never happen? how do i report this?
		error(tc, fn.declared_return_type.tok, "invalid function return type")
		return
	}

	// If no return type given, assume void
	if declared_return_type == .Any || declared_return_type == .Void {
		declared_return_type = .Void
	}

	// main function is special, check its return type
	// or implicitly set it to Int if no type is given
	if fn.name.value == "main" {
		tc.has_main = true
		if declared_return_type == .Void {
			declared_return_type = .Int
		} else if declared_return_type != .Int {
			error(
				tc,
				fn.declared_return_type.tok,
				"Expected return type of Int, got %s",
				declared_return_type,
			)
		}
	}

	param_types := make([dynamic]^ast.TypeInfo, tc.allocator)
	for param in fn.args {
		append(&param_types, param.resolved_type)
	}
	fn.resolved_type = make_typeinfo(tc, .Function)
	fn.resolved_type.data = ast.FunctionTypeInfo {
		param_types = param_types,
		return_type = make_typeinfo(tc, declared_return_type),
	}
	add_symbol(tc, fn.name.value, fn.resolved_type)
}

check_stmt :: proc(tc: ^TypeChecker, stmt: ast.Statement) {
	switch s in stmt {
	case ^ast.ExprStatement:
		check_expr(tc, s.value)

	case ^ast.FunctionStatement:
		check_func_stmt(tc, s)

	case ^ast.ReturnStatement:
		expr_type := check_expr(tc, s.value)
		s.resolved_type = expr_type

	case ^ast.AssignStatement:
		check_assign_stmt(tc, s)

	case ^ast.ReassignStatement:
		check_reassign_stmt(tc, s)

	case ^ast.IfStatement:
		check_if_stmt(tc, s)

	case ^ast.LoopStatement:
	case ^ast.BreakStatement:
		error(tc, s.tok, "TODO: not implemented")

	case ^ast.Program, ^ast.FunctionArg, ^ast.BlockStatement:
		logger.error("Unreachable statement: %+v", stmt)
	}
}

check_if_stmt :: proc(tc: ^TypeChecker, s: ^ast.IfStatement) {
	cond_typeinfo := check_expr(tc, s.condition)
	if cond_typeinfo.kind != .Bool {
		error(
			tc,
			ast.get_token_from_expr(s.condition),
			"if expression should resolve to a boolean, got %s",
			cond_typeinfo.kind,
		)
		return
	}

	symbols_push_scope(tc)
	for stmt in s.consequence.stmts {
		check_stmt(tc, stmt)
	}
	symbols_pop_scope(tc)

	if s.alternative != nil {
		symbols_push_scope(tc)
		for stmt in s.alternative.stmts {
			check_stmt(tc, stmt)
		}
		symbols_pop_scope(tc)
	}
	return
}

check_reassign_stmt :: proc(tc: ^TypeChecker, s: ^ast.ReassignStatement) {
	sym_type, found := lookup_symbol(tc, s.name.value)
	if !found {
		error(tc, s.tok, "%s is not defined, to assign a new variable use :=", s.name.value)
		return
	}
	expr_type := check_expr(tc, s.value)
	if sym_type.kind != expr_type.kind {
		error(
			tc,
			s.tok,
			"Type mismatch - %s is type %s, cannot reassign to type %s",
			s.name.value,
			sym_type.kind,
			expr_type.kind,
		)
		return
	}
	s.resolved_type = sym_type
	return
}

check_assign_stmt :: proc(tc: ^TypeChecker, s: ^ast.AssignStatement) {
	_, found := lookup_symbol(tc, s.name.value)
	if found {
		error(tc, s.tok, "Symbol already declared: %s", s.name.value)
		s.resolved_type = make_typeinfo(tc, .Invalid)
		return
	}

	declared_type: ast.TypeKind = .Any
	if s.declared_type != nil {
		declared_type = check_type_annotation(tc, s.declared_type)
		if declared_type == .Invalid {
			error(tc, s.tok, "Invalid type - %s", s.declared_type.tok.type)
			s.resolved_type = make_typeinfo(tc, .Invalid)
			return
		}
	}

	if s.value == nil {
		s.resolved_type = make_typeinfo(tc, declared_type)
	} else {
		expr_type := check_expr(tc, s.value)
		if declared_type == .Any {
			s.resolved_type = expr_type
		} else if declared_type != expr_type.kind {
			error(
				tc,
				s.tok,
				"Type mismatch - declared type: %s, expression type: %s",
				declared_type,
				expr_type.kind,
			)
			s.resolved_type = make_typeinfo(tc, .Invalid)
			return
		} else {
			s.resolved_type = expr_type
		}
	}

	add_symbol(tc, s.name.value, s.resolved_type)
}

check_func_stmt :: proc(tc: ^TypeChecker, fn: ^ast.FunctionStatement) {
	// Since we already checked the function signature, all we need to do is check the body
	symbols_push_scope(tc)
	defer symbols_pop_scope(tc)

	for arg in fn.args {
		add_symbol(tc, arg.ident.value, arg.resolved_type)
	}

	if fn.body == nil {
		error(tc, fn.tok, "Missing function body")
		return
	}

	typeinfo, ok := fn.resolved_type.data.(ast.FunctionTypeInfo)
	if !ok {
		error(tc, fn.tok, "expected functiontypeinfo, got %s", fn.resolved_type.kind)
		return
	}

	check_body_stmt_for_returns(tc, fn.body, typeinfo.return_type.kind)
	return
}

check_body_stmt_for_returns :: proc(
	tc: ^TypeChecker,
	body: ^ast.BlockStatement,
	declared_return_type: ast.TypeKind,
) {
	for stmt in body.stmts {
		if _, ok := stmt.(^ast.FunctionStatement); ok {
			error(tc, body.tok, "nested functions not allowed")
		}
		check_stmt(tc, stmt)
		#partial switch s in stmt {
		case ^ast.ReturnStatement:
			if s.resolved_type.kind != declared_return_type {
				error(
					tc,
					s.tok,
					"Expected return type %s, got %s",
					declared_return_type,
					s.resolved_type.kind,
				)
			}
		case ^ast.IfStatement:
			check_body_stmt_for_returns(tc, s.consequence, declared_return_type)
			if s.alternative != nil {
				check_body_stmt_for_returns(tc, s.alternative, declared_return_type)
			}
		}
	}
}

check_expr :: proc(tc: ^TypeChecker, expr: ast.Expr) -> ^ast.TypeInfo {
	switch e in expr {
	case ^ast.Boolean:
		e.resolved_type = make_typeinfo(tc, .Bool)
		return e.resolved_type
	case ^ast.IntLiteral:
		e.resolved_type = make_typeinfo(tc, .Int)
		return e.resolved_type
	case ^ast.StringLiteral:
		e.resolved_type = make_typeinfo(tc, .String)
		return e.resolved_type
	case ^ast.CallExpr:
		return check_call_expr(tc, e)
	case ^ast.InfixExpr:
		return check_infix_expr(tc, e)
	case ^ast.PrefixExpr:
		rhs_type := check_expr(tc, e.right)
		if rhs_type.kind == .Invalid {
			return rhs_type
		}
		e.resolved_type = rhs_type
		return e.resolved_type

	case ^ast.Identifier:
		symbol_type, found := lookup_symbol(tc, e.value)
		if !found {
			error(tc, e.tok, "Identifier not found: %s", e.value)
			e.resolved_type = make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
		e.resolved_type = symbol_type
		return e.resolved_type
	case ^ast.Array:
		logger.error("TODO: typechecking arrays not implemented")
		e.resolved_type = make_typeinfo(tc, .Array)
		return e.resolved_type
	}
	logger.error("Unreachable - checking expr: %+v", expr)
	return nil
}

check_infix_expr :: proc(tc: ^TypeChecker, e: ^ast.InfixExpr) -> ^ast.TypeInfo {
	lhs_type := check_expr(tc, e.left)
	rhs_type := check_expr(tc, e.right)
	if lhs_type.kind == .Invalid || rhs_type.kind == .Invalid {
		error(
			tc,
			e.tok,
			"infix expression contains invalid left: %s, right: %s",
			lhs_type.kind,
			rhs_type.kind,
		)
		e.resolved_type = make_typeinfo(tc, .Invalid)
		return e.resolved_type
	}
	if lhs_type.kind != rhs_type.kind {
		error(tc, e.tok, "type mismatch - expected %s, got %s", lhs_type.kind, rhs_type.kind)
		e.resolved_type = make_typeinfo(tc, .Invalid)
		return e.resolved_type
	}
	#partial switch e.tok.type {
	case .EQ, .NOT_EQ, .LT, .GT, .LTE, .GTE:
		e.resolved_type = make_typeinfo(tc, .Bool)
	case:
		e.resolved_type = lhs_type
	}
	return e.resolved_type

}

check_call_expr :: proc(tc: ^TypeChecker, e: ^ast.CallExpr) -> ^ast.TypeInfo {
	typeinfo, found := lookup_symbol(tc, e.func.value)
	if !found {
		// TODO: add better builtin support
		if e.func.value == "printf" {
			// TODO: do i need to support variadics now?? (builtins)
			return_type := make_typeinfo(tc, .Int)
			f_typeinfo := make_typeinfo(tc, .Function)
			f_typeinfo.data = ast.FunctionTypeInfo {
				return_type = return_type,
			}
			e.resolved_type = f_typeinfo
			return e.resolved_type
		}

		error(tc, e.tok, "Identifier not found: %s", e.func.value)
		return make_typeinfo(tc, .Invalid)
	}

	if typeinfo.kind != .Function {
		error(tc, e.tok, "Identifier is not a function, got %s", typeinfo.kind)
		return make_typeinfo(tc, .Invalid)
	}

	f_typeinfo := typeinfo.data.(ast.FunctionTypeInfo)

	if len(f_typeinfo.param_types) != len(e.args) {
		logger.info("%+v", f_typeinfo)
		error(
			tc,
			e.tok,
			"function arity does not match. Signature expects %d parameters, got %d",
			len(f_typeinfo.param_types),
			len(e.args),
		)
		return make_typeinfo(tc, .Invalid)
	}

	for arg, i in e.args {
		arg_typeinfo := check_expr(tc, arg)
		if arg_typeinfo.kind == .Invalid {
			error(tc, ast.get_token_from_expr(arg), "invalid function parameter")
			return make_typeinfo(tc, .Invalid)
		}
		f_param := f_typeinfo.param_types[i]
		if arg_typeinfo.kind != f_param.kind {
			error(
				tc,
				ast.get_token_from_expr(arg),
				"parameter type mismatch: expected %s, got %s",
				f_param.kind,
				arg_typeinfo.kind,
			)
			return make_typeinfo(tc, .Invalid)
		}
	}
	e.resolved_type = f_typeinfo.return_type
	return e.resolved_type
}


check_type_annotation :: proc(tc: ^TypeChecker, a: ^ast.TypeAnnotation) -> ast.TypeKind {
	if a == nil {
		return .Any
	}

	#partial switch a.tok.type {
	case .TYPE_INT:
		return .Int
	case .TYPE_STRING:
		return .String
	case .TYPE_BOOL:
		return .Bool
	case .TYPE_VOID:
		return .Void
	case .L_BRACKET:
		return .Array
	}
	return .Invalid
}

error :: proc(tc: ^TypeChecker, tok: token.Token, ft: string, args: ..any) {
	// TODO: I feel like this shouldn't use the temporary allocator
	msg := fmt.tprintf(ft, ..args)
	err := logger.CompilerError {
		msg  = msg,
		line = tok.line,
		col  = tok.col,
	}
	append(&tc.errors, err)
}

add_symbol :: proc(tc: ^TypeChecker, name: string, typeinfo: ^ast.TypeInfo) {
	if len(tc.symbols) <= 0 {
		logger.error("can't add symbol to an empty stack")
		return
	}
	name_copy := strings.clone(name, tc.allocator)
	tc.symbols[len(tc.symbols) - 1][name_copy] = typeinfo
}

lookup_symbol :: proc(tc: ^TypeChecker, name: string) -> (^ast.TypeInfo, bool) {
	for i := len(tc.symbols) - 1; i >= 0; i -= 1 {
		scope := tc.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

symbols_pop_scope :: proc(tc: ^TypeChecker) {
	if len(tc.symbols) <= 0 {
		logger.error("attemping to pop scope from empty symbols table")
		return
	}
	popped := pop(&tc.symbols)
	delete(popped)
}

symbols_push_scope :: proc(tc: ^TypeChecker) {
	scope := make(map[string]^ast.TypeInfo, tc.allocator)
	append(&tc.symbols, scope)
}

make_typeinfo :: proc(tc: ^TypeChecker, kind: ast.TypeKind) -> ^ast.TypeInfo {
	typeinfo := new(ast.TypeInfo, tc.allocator)
	typeinfo.kind = kind
	return typeinfo
}
