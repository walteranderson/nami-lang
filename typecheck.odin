package main

import "core:fmt"
import "core:mem"

TypeChecker :: struct {
	errs:      [dynamic]string,
	allocator: mem.Allocator,
	program:   ^Program,
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
	case ^BlockStatement:
	case ^ReturnStatement:
	case ^AssignStatement:
	case ^ReassignStatement:
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
