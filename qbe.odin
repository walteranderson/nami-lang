package main

import "core:fmt"
import "core:mem"
import os "core:os/os2"
import "core:strings"

Qbe :: struct {
	allocator:               mem.Allocator,
	sb:                      strings.Builder,
	program:                 ^Program,
	errors:                  [dynamic]string,
	strs:                    map[string]string,
	symbols:                 [dynamic]map[string]string,
	current_func_temp_count: int,
}

qbe_init :: proc(qbe: ^Qbe, program: ^Program, allocator: mem.Allocator) {
	qbe.program = program
	qbe.allocator = allocator
	qbe.errors = make([dynamic]string, 0, allocator)
	strings.builder_init(&qbe.sb, allocator)

	qbe.symbols = make([dynamic]map[string]string, 0, allocator)
	qbe_push_scope(qbe)
}

qbe_generate :: proc(qbe: ^Qbe) {
	for stmt in qbe.program.stmts {
		qbe_gen_stmt(qbe, stmt)
	}

	if len(qbe.strs) > 0 {
		for content, label in qbe.strs {
			qbe_emit(qbe, "data %s = {{ b \"%s\" }}\n", label, content)
		}
	}
}

qbe_gen_stmt :: proc(qbe: ^Qbe, stmt: Statement) {
	switch s in stmt {
	case ^AssignStatement:
		reg := qbe_gen_expr(qbe, s.value)
		qbe_add_symbol(qbe, s.name.value, reg)
	case ^ReassignStatement:
	//
	case ^ExprStatement:
		qbe_gen_expr(qbe, s.value)
	case ^BlockStatement:
		for st in s.stmts {
			qbe_gen_stmt(qbe, st)
		}
	case ^ReturnStatement:
		if s.value != nil {
			expr_reg := qbe_gen_expr(qbe, s.value)
			qbe_emit(qbe, "  ret %s\n", expr_reg)
		} else {
			qbe_emit(qbe, "  ret\n")
		}
	case ^Program:
		qbe_error(qbe, "Unexpected program")
	}
}

qbe_gen_expr :: proc(qbe: ^Qbe, expr: Expr) -> string {
	switch v in expr {
	case ^InfixExpr:
	//
	case ^PrefixExpr:
	//
	case ^CallExpr:
	//
	case ^StringLiteral:
	//
	case ^Function:
		return qbe_gen_func(qbe, v)
	case ^FunctionArg:
	//
	case ^Identifier:
		ident_reg, found := qbe_lookup_symbol(qbe, v.value)
		if !found {
			qbe_error(qbe, "undefined identifier: %s", v.value)
			return ""
		}
		reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w loadw %s\n", reg, ident_reg)
		return reg
	case ^IntLiteral:
		reg_name := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =l alloc4 4\n", reg_name)
		qbe_emit(qbe, "  storew %d, %s\n", v.value, reg_name)
		return reg_name
	case ^Boolean:
	//
	}
	return ""
}

qbe_gen_func :: proc(qbe: ^Qbe, fn: ^Function) -> string {
	qbe_push_scope(qbe)
	qbe.current_func_temp_count = 0

	if fn.name.value == "main" {
		qbe_emit(qbe, "export function w $main(")
	} else {
		qbe_emit(qbe, "function w $%s(", fn.name.value)
	}

	// TODO: args
	qbe_emit(qbe, ") {{\n")

	qbe_emit(qbe, "@start\n")

	qbe_gen_stmt(qbe, fn.body)

	qbe_emit(qbe, "}}\n")

	return fmt.tprintf("$%s", fn.name.value)
}

qbe_new_temp_reg :: proc(qbe: ^Qbe) -> string {
	qbe.current_func_temp_count += 1
	name := fmt.tprintf("%%.%d", qbe.current_func_temp_count)
	return name
}

qbe_emit :: proc(qbe: ^Qbe, format: string, args: ..any) {
	fmt.sbprintf(&qbe.sb, format, ..args)
}

qbe_error :: proc(qbe: ^Qbe, ft: string, args: ..any) {
	append(&qbe.errors, fmt.tprintf(ft, ..args))
}

qbe_push_scope :: proc(qbe: ^Qbe) {
	scope := make(map[string]string, qbe.allocator)
	append(&qbe.symbols, scope)
}

qbe_pop_scope :: proc(qbe: ^Qbe) {
	if len(qbe.symbols) <= 0 {
		qbe_error(qbe, "attempting to pop scope from an empty symbol stack")
		return
	}
	popped := pop(&qbe.symbols)
	delete(popped)
}

qbe_add_symbol :: proc(qbe: ^Qbe, name: string, reg: string) {
	if len(qbe.symbols) <= 0 {
		qbe_error(qbe, "can't add symbols to an empty stack")
		return
	}
	name_cpy := strings.clone(name, qbe.allocator)
	reg_cpy := strings.clone(reg, qbe.allocator)
	qbe.symbols[len(qbe.symbols) - 1][name_cpy] = reg_cpy
}

qbe_lookup_symbol :: proc(qbe: ^Qbe, name: string) -> (string, bool) {
	for i := len(qbe.symbols) - 1; i >= 0; i -= 1 {
		scope := qbe.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return "", false
}

qbe_compile :: proc(qbe: ^Qbe, program_name: string) -> (err: os.Error) {
	qbe_file := create_file_name(program_name, "ssa")
	asm_file := create_file_name(program_name, "s")

	content := strings.to_string(qbe.sb)
	os.write_entire_file(qbe_file, transmute([]byte)(content)) or_return

	qbe_cmd := []string{"qbe", "-o", asm_file, qbe_file}
	log(.INFO, "CMD: %v", qbe_cmd)

	qbe_desc := os.Process_Desc {
		command = qbe_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(qbe_desc) or_return

	cc_cmd := []string{"cc", "-o", program_name, asm_file}
	log(.INFO, "CMD: %v", cc_cmd)

	cc_desc := os.Process_Desc {
		command = cc_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(cc_desc) or_return

	return nil
}
