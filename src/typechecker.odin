package nami

import "core:fmt"
import "core:mem"
import "core:strings"
import "core:time"

TypeChecker :: struct {
	errs:      [dynamic]string,
	allocator: mem.Allocator,
	program:   ^Program,
	symbols:   [dynamic]map[string]^TypeInfo,
	has_main:  bool,
}

tc_init :: proc(tc: ^TypeChecker, p: ^Program, allocator: mem.Allocator) {
	tc.program = p
	tc.allocator = allocator
	tc.errs = make([dynamic]string, allocator)
	tc_symbols_push_scope(tc)
}

tc_check_program :: proc(tc: ^TypeChecker) {
	start := time.now()
	log(.INFO, "Typechecking program")
	for stmt in tc.program.stmts {
		tc_check_stmt(tc, stmt)
	}

	if !tc.has_main {
		tc_error(tc, "Missing main function")
	}

	log(.INFO, "Typechecking complete: %v", time.diff(start, time.now()))
}

tc_check_stmt :: proc(tc: ^TypeChecker, stmt: Statement) {
	switch s in stmt {
	case ^Program:
		tc_error(tc, "Unexpected program statement")
	case ^ExprStatement:
		tc_check_expr(tc, s.value)
		return
	case ^FunctionStatement:
		is_main := false
		if s.name.value == "main" {
			if tc.has_main {
				tc_error(tc, "Main function already declared")
			} else {
				tc.has_main = true
				is_main = true
			}
		}

		tc_symbols_push_scope(tc)

		for arg in s.args {
			arg.resolved_type = tc_make_typeinfo(
				tc,
				tc_check_type_annotation(tc, arg.declared_type),
			)
			tc_add_symbol(tc, arg.ident.value, arg.resolved_type)
		}

		if s.body == nil {
			tc_error(tc, "Missing function body")
			tc_symbols_pop_scope(tc)
			return
		}

		declared_return_type := tc_check_type_annotation(tc, s.declared_return_type)
		if declared_return_type == .Invalid || declared_return_type == .Any {
			tc_error(tc, "missing function return type")
			tc_symbols_pop_scope(tc)
			return
		}

		if is_main && declared_return_type != .Int {
			tc_error(tc, "Main function must have return type of int")
			tc_symbols_pop_scope(tc)
			return
		}

		tc_check_body_stmt_for_returns(tc, s.body, declared_return_type)

		tc_symbols_pop_scope(tc)

		param_types: [dynamic]^TypeInfo
		for param in s.args {
			append(&param_types, param.resolved_type)
		}
		s.resolved_type = tc_make_typeinfo(tc, .Function)
		s.resolved_type.data = FunctionTypeInfo {
			param_types = param_types,
			return_type = tc_make_typeinfo(tc, declared_return_type),
		}
		tc_add_symbol(tc, s.name.value, s.resolved_type)
		return

	case ^BlockStatement:
		tc_error(tc, "Unreachable - block statement")
	case ^FunctionArg:
		tc_error(tc, "Unreachable - function arg")

	case ^ReturnStatement:
		expr_type := tc_check_expr(tc, s.value)
		s.resolved_type = expr_type
		return

	case ^AssignStatement:
		_, found := tc_lookup_symbol(tc, s.name.value)
		if found {
			tc_error(tc, "Symbol already declared: %s", s.name.value)
			s.resolved_type = tc_make_typeinfo(tc, .Invalid)
			return
		}

		declared_type: TypeKind = .Any
		if s.declared_type != nil {
			declared_type = tc_check_type_annotation(tc, s.declared_type)
			if declared_type == .Invalid {
				tc_error(tc, "Invalid type - %s", s.declared_type.name)
				s.resolved_type = tc_make_typeinfo(tc, .Invalid)
				return
			}
		}

		if s.value == nil {
			s.resolved_type = tc_make_typeinfo(tc, declared_type)
		} else {
			expr_type := tc_check_expr(tc, s.value)
			if declared_type == .Any {
				s.resolved_type = expr_type
			} else if declared_type != expr_type.kind {
				tc_error(
					tc,
					"Type mismatch - declared type: %s, expression type: %s",
					declared_type,
					expr_type,
				)
				s.resolved_type = tc_make_typeinfo(tc, .Invalid)
				return
			} else {
				s.resolved_type = expr_type
			}
		}

		tc_add_symbol(tc, s.name.value, s.resolved_type)
		return

	case ^ReassignStatement:
		sym_type, found := tc_lookup_symbol(tc, s.name.value)
		if !found {
			tc_error(tc, "%s is not defined", s.name.value)
			return
		}
		expr_type := tc_check_expr(tc, s.value)
		if sym_type.kind != expr_type.kind {
			tc_error(
				tc,
				"Type mismatch - %s is type %s, cannot reassign to type %s",
				s.name.value,
				sym_type.kind,
				expr_type.kind,
			)
			return
		}
		s.resolved_type = sym_type
		return

	case ^IfStatement:
		cond_typeinfo := tc_check_expr(tc, s.condition)
		if cond_typeinfo.kind != .Bool {
			tc_error(tc, "if expression should resolve to a boolean, got %s", cond_typeinfo.kind)
			return
		}

		tc_symbols_push_scope(tc)
		for stmt in s.consequence.stmts {
			tc_check_stmt(tc, stmt)
		}
		tc_symbols_pop_scope(tc)

		if s.alternative != nil {
			tc_symbols_push_scope(tc)
			for stmt in s.alternative.stmts {
				tc_check_stmt(tc, stmt)
			}
			tc_symbols_pop_scope(tc)
		}
		return
	}
	log(.ERROR, "Unreachable typechecking statement: %+v", stmt)
	return
}

tc_check_expr :: proc(tc: ^TypeChecker, expr: Expr) -> ^TypeInfo {
	switch e in expr {
	case ^Boolean:
		e.resolved_type = tc_make_typeinfo(tc, .Bool)
		return e.resolved_type
	case ^IntLiteral:
		e.resolved_type = tc_make_typeinfo(tc, .Int)
		return e.resolved_type
	case ^StringLiteral:
		e.resolved_type = tc_make_typeinfo(tc, .String)
		return e.resolved_type
	case ^CallExpr:
		typeinfo, found := tc_lookup_symbol(tc, e.func.value)
		if !found {
			// TODO: add better builtin support
			if e.func.value == "printf" {
				// TODO: do i need to support variadics now?? (builtins)
				return_type := tc_make_typeinfo(tc, .Int)
				f_typeinfo := tc_make_typeinfo(tc, .Function)
				f_typeinfo.data = FunctionTypeInfo {
					return_type = return_type,
				}
				e.resolved_type = f_typeinfo
				return e.resolved_type
			}

			tc_error(tc, "Identifier not found: %s", e.func.value)
			return tc_make_typeinfo(tc, .Invalid)
		}

		if typeinfo.kind != .Function {
			tc_error(tc, "Identifier is not a function, got %s", typeinfo.kind)
			return tc_make_typeinfo(tc, .Invalid)
		}

		f_typeinfo := typeinfo.data.(FunctionTypeInfo)

		if len(f_typeinfo.param_types) != len(e.args) {
			tc_error(
				tc,
				"function arity does not match. Signature expects %d parameters, got %d",
				len(f_typeinfo.param_types),
				len(e.args),
			)
			return tc_make_typeinfo(tc, .Invalid)
		}

		for arg, i in e.args {
			arg_typeinfo := tc_check_expr(tc, arg)
			if arg_typeinfo.kind == .Invalid {
				tc_error(tc, "invalid function parameter")
				return tc_make_typeinfo(tc, .Invalid)
			}
			f_param := f_typeinfo.param_types[i]
			if arg_typeinfo.kind != f_param.kind {
				tc_error(
					tc,
					"parameter type mismatch: expected %s, got %s",
					f_param.kind,
					arg_typeinfo.kind,
				)
				return tc_make_typeinfo(tc, .Invalid)
			}
		}
		e.resolved_type = f_typeinfo.return_type
		return e.resolved_type
	case ^InfixExpr:
		lhs_type := tc_check_expr(tc, e.left)
		rhs_type := tc_check_expr(tc, e.right)
		if lhs_type.kind == .Invalid || rhs_type.kind == .Invalid {
			tc_error(
				tc,
				"infix expression contains invalid left: %s, right: %s",
				lhs_type,
				rhs_type,
			)
			e.resolved_type = tc_make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
		if lhs_type.kind != rhs_type.kind {
			tc_error(tc, "type mismatch - expected %s, got %s", lhs_type.kind, rhs_type.kind)
			e.resolved_type = tc_make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
		#partial switch e.tok.type {
		case .EQ, .NOT_EQ, .LT, .GT:
			e.resolved_type = tc_make_typeinfo(tc, .Bool)
		case:
			e.resolved_type = lhs_type
		}
		return e.resolved_type

	case ^PrefixExpr:
		rhs_type := tc_check_expr(tc, e.right)
		if rhs_type.kind == .Invalid {
			return rhs_type
		}
		e.resolved_type = rhs_type
		return e.resolved_type

	case ^Identifier:
		symbol_type, found := tc_lookup_symbol(tc, e.value)
		if !found {
			tc_error(tc, "Identifier not found: %s", e.value)
			e.resolved_type = tc_make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
		e.resolved_type = symbol_type
		return e.resolved_type

	}
	log(.ERROR, "Unreachable - checking expr: %+v", expr)
	return nil
}

tc_check_body_stmt_for_returns :: proc(
	tc: ^TypeChecker,
	body: ^BlockStatement,
	declared_return_type: TypeKind,
) {
	for stmt in body.stmts {
		if _, ok := stmt.(^FunctionStatement); ok {
			tc_error(tc, "nested functions not allowed")
		}
		tc_check_stmt(tc, stmt)
		#partial switch s in stmt {
		case ^ReturnStatement:
			if s.resolved_type.kind != declared_return_type {
				tc_error(
					tc,
					"Expected return type %s, got %s",
					declared_return_type,
					s.resolved_type.kind,
				)
			}
		case ^IfStatement:
			tc_check_body_stmt_for_returns(tc, s.consequence, declared_return_type)
			if s.alternative != nil {
				tc_check_body_stmt_for_returns(tc, s.alternative, declared_return_type)
			}
		}
	}
}

tc_check_type_annotation :: proc(tc: ^TypeChecker, a: ^TypeAnnotation) -> TypeKind {
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
	case:
		return .Invalid
	}
	return .Invalid
}

tc_error :: proc(tc: ^TypeChecker, ft: string, args: ..any) {
	append(&tc.errs, fmt.tprintf(ft, ..args))
}

tc_add_symbol :: proc(tc: ^TypeChecker, name: string, typeinfo: ^TypeInfo) {
	if len(tc.symbols) <= 0 {
		tc_error(tc, "can't add symbol to an empty stack")
		return
	}
	name_copy := strings.clone(name, tc.allocator)
	tc.symbols[len(tc.symbols) - 1][name_copy] = typeinfo
}

tc_lookup_symbol :: proc(tc: ^TypeChecker, name: string) -> (^TypeInfo, bool) {
	for i := len(tc.symbols) - 1; i >= 0; i -= 1 {
		scope := tc.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

tc_symbols_pop_scope :: proc(tc: ^TypeChecker) {
	if len(tc.symbols) <= 0 {
		tc_error(tc, "attemping to pop scope from empty symbols table")
		return
	}
	popped := pop(&tc.symbols)
	delete(popped)
}

tc_symbols_push_scope :: proc(tc: ^TypeChecker) {
	scope := make(map[string]^TypeInfo, tc.allocator)
	append(&tc.symbols, scope)
}

tc_make_typeinfo :: proc(tc: ^TypeChecker, kind: TypeKind) -> ^TypeInfo {
	typeinfo := new(TypeInfo, tc.allocator)
	typeinfo.kind = kind
	return typeinfo
}
