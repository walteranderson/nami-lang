package main

import "core:fmt"
import "core:mem"
import "core:strings"

TypeChecker :: struct {
	errs:      [dynamic]string,
	allocator: mem.Allocator,
	program:   ^Program,
	symbols:   [dynamic]map[string]Type,
}

tc_init :: proc(tc: ^TypeChecker, p: ^Program, allocator: mem.Allocator) {
	tc.program = p
	tc.allocator = allocator
	tc.errs = make([dynamic]string, allocator)
}

tc_check_program :: proc(tc: ^TypeChecker) {
	for stmt in tc.program.stmts {
		tc_check_stmt(tc, stmt)
	}
}

tc_check_stmt :: proc(tc: ^TypeChecker, stmt: Statement) {
	switch s in stmt {
	case ^Program:
		tc_error(tc, "Unexpected program statement")
		return
	case ^ExprStatement:
	//
	case ^BlockStatement:
	//
	case ^ReturnStatement:
	//
	case ^AssignStatement:
		_, found := tc_lookup_symbol(tc, s.name.value)
		if found {
			tc_error(tc, "Symbol already declared: %s", s.name.value)
			s.resolved_type = .Invalid
			return
		}
		expr_type := tc_check_expr(tc, s.value)
		if s.declared_type == nil {
			s.resolved_type = expr_type
			return
		}
		declared_type := tc_check_type_annotation(tc, s.declared_type)
		if declared_type == .Invalid {
			tc_error(tc, "Invalid type - %s", s.declared_type.name)
			s.resolved_type = .Invalid
			return
		}
		if declared_type != expr_type {
			tc_error(
				tc,
				"Type mismatch: declared type: %s - expression type: %s",
				declared_type,
				expr_type,
			)
			s.resolved_type = .Invalid
			return
		}
		tc_add_symbol(tc, s.name.value, s.resolved_type)

	case ^ReassignStatement:
	//
	}
}

tc_check_expr :: proc(tc: ^TypeChecker, expr: Expr) -> Type {
	switch e in expr {
	case ^Boolean:
		e.resolved_type = .Bool
		return e.resolved_type
	case ^IntLiteral:
		e.resolved_type = .Int
		return e.resolved_type
	case ^StringLiteral:
		e.resolved_type = .String
		return e.resolved_type
	case ^CallExpr:
	// lookup function in symbol table to get return type
	// or check against builtins (put in some kind of structure, aka printf)
	case ^Function:
	//
	case ^InfixExpr:
		lhs_type := tc_check_expr(tc, e.left)
		rhs_type := tc_check_expr(tc, e.right)
		if lhs_type == .Invalid || rhs_type == .Invalid {
			return .Invalid
		}
		if lhs_type != rhs_type {
			tc_error(tc, "type mismatch - expected %s, got %s", lhs_type, rhs_type)
			return .Invalid
		}
		e.resolved_type = lhs_type
		return e.resolved_type
	case ^PrefixExpr:
		rhs_type := tc_check_expr(tc, e.right)
		if rhs_type == .Invalid {
			return .Invalid
		}
		e.resolved_type = rhs_type
		return e.resolved_type
	case ^Identifier:
		symbol_type, found := tc_lookup_symbol(tc, e.value)
		if !found {
			tc_error(tc, "Identifier not found: %s", e.value)
			return .Invalid
		}
		e.resolved_type = symbol_type
		return e.resolved_type
	case ^FunctionArg:
		e.resolved_type = tc_check_type_annotation(tc, e.declared_type)
		return e.resolved_type

	}
	return .Invalid
}

tc_check_type_annotation :: proc(tc: ^TypeChecker, a: ^TypeAnnotation) -> Type {
	if a == nil {
		return .Any
	}

	switch a.name {
	case "int":
		return .Int
	case "string":
		return .String
	case "bool":
		return .Bool
	case "void":
		return .Void
	case:
		return .Invalid
	}
}

tc_error :: proc(tc: ^TypeChecker, ft: string, args: ..any) {
	append(&tc.errs, fmt.tprintf(ft, ..args))
}

tc_add_symbol :: proc(tc: ^TypeChecker, name: string, type: Type) {
	if len(tc.symbols) <= 0 {
		tc_error(tc, "can't add symbol to an empty stack")
		return
	}
	name_copy := strings.clone(name, tc.allocator)
	tc.symbols[len(tc.symbols) - 1][name_copy] = type
}

tc_lookup_symbol :: proc(tc: ^TypeChecker, name: string) -> (Type, bool) {
	for i := len(tc.symbols) - 1; i >= 0; i -= 1 {
		scope := tc.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return .Invalid, false
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
	scope := make(map[string]Type, tc.allocator)
	append(&tc.symbols, scope)
}
