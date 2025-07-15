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
	str_count:               int,
	symbols:                 [dynamic]map[string]string,
	functions:               map[string]string,
	current_func_temp_count: int,
}

QbeType :: enum {
	Invalid,
	Word, // 32-bit integer
	Long, // 64-bit integer or pointer/address
	Byte, // 8-bit integer
	Half, // 16-bit integer
	Float, // Single-precision float
	Double, // Double-precision float
}

qbe_type_to_string :: proc(type: QbeType) -> string {
	switch type {
	case .Word:
		return "w"
	case .Long:
		return "l"
	case .Byte:
		return "b"
	case .Half:
		return "h"
	case .Float:
		return "s"
	case .Double:
		return "d"
	case .Invalid:
		return ""
	}
	return ""
}

QbeResult :: struct {
	value: string,
	type:  QbeType,
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
		for key, &value in qbe.strs {
			qbe_emit(qbe, "data %s = {{ b \"%s\" }}\n", key, value)
		}
	}
}

qbe_gen_stmt :: proc(qbe: ^Qbe, stmt: Statement) {
	switch s in stmt {
	case ^AssignStatement:
		reg_ptr := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =l alloc4 4\n", reg_ptr)
		qbe_add_symbol(qbe, s.name.value, reg_ptr)
		res := qbe_gen_expr(qbe, s.value)
		qbe_emit(qbe, "  store%s %s, %s\n", res.type, res.value, reg_ptr)
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
			res := qbe_gen_expr(qbe, s.value)
			qbe_emit(qbe, "  ret %s\n", res.value)
		} else {
			qbe_emit(qbe, "  ret\n")
		}
	case ^Program:
		qbe_error(qbe, "Unexpected program")
	}
}

qbe_gen_expr :: proc(qbe: ^Qbe, expr: Expr) -> QbeResult {
	switch v in expr {
	case ^InfixExpr:
		lhs := qbe_gen_expr(qbe, v.left)
		rhs := qbe_gen_expr(qbe, v.right)
		if lhs.type == .Invalid || rhs.type == .Invalid {
			return QbeResult{"", .Invalid}
		}

		op_str := ""
		switch v.op {
		case "+":
			op_str = "add"
		case "-":
			op_str = "sub"
		case "*":
			op_str = "mul"
		case "/":
			op_str = "div"
		case:
			qbe_error(qbe, "Unsupported operator: %s", v.op)
			return QbeResult{"", .Invalid}
		}

		res_type: QbeType
		if lhs.type == .Word && rhs.type == .Word {
			res_type = .Word
		} else if lhs.type == .Long && rhs.type == .Long {
			res_type = .Long
		} else {
			qbe_error(
				qbe,
				"type mismatch for operator %s: cannot operate on %s and %s",
				op_str,
				lhs.type,
				rhs.type,
			)
			return QbeResult{"", .Invalid}
		}

		reg := qbe_new_temp_reg(qbe)
		qbe_emit(
			qbe,
			"  %s =%s %s %s, %s\n",
			reg,
			qbe_type_to_string(res_type),
			op_str,
			lhs.value,
			rhs.value,
		)
		return QbeResult{reg, res_type}

	case ^PrefixExpr:
	//
	case ^CallExpr:
		// TODO: expand this further to be less hard-coded
		ret_type: QbeType
		if v.func.value == "printf" {
			ret_type = .Long
		} else if v.func.value == "main" {
			ret_type = .Word
		} else {
			ret_type = .Word
		}

		args_reg := make([dynamic]QbeResult, qbe.allocator)
		defer delete(args_reg)
		for arg in v.args {
			reg := qbe_gen_expr(qbe, arg)
			if reg.type == .Invalid {
				return QbeResult{"", .Invalid}
			}
			append(&args_reg, reg)
		}

		ret_reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =%s call $%s(", ret_reg, qbe_type_to_string(ret_type), v.func.value)
		for arg, i in args_reg {
			qbe_emit(qbe, "%s %s", qbe_type_to_string(arg.type), arg.value)
			if i + 1 < len(args_reg) {
				qbe_emit(qbe, ", ")
			}
		}
		qbe_emit(qbe, ")\n")
		return QbeResult{ret_reg, ret_type}

	case ^StringLiteral:
		qbe.str_count += 1

		label_sb: strings.Builder
		strings.builder_init(&label_sb, qbe.allocator)
		defer strings.builder_destroy(&label_sb)
		fmt.sbprintf(&label_sb, "$str_%d", qbe.str_count)
		label := strings.to_string(label_sb)

		val_sb: strings.Builder
		strings.builder_init(&val_sb, qbe.allocator)
		defer strings.builder_destroy(&val_sb)
		fmt.sbprintf(&val_sb, "%s\\00", v.value)
		val := strings.to_string(val_sb)

		qbe.strs[label] = val
		return QbeResult{label, .Long}
	case ^Function:
		// TODO: move function definition generation in a previous pass-through
		// and make this just return the function name symbol
		return qbe_gen_func_def(qbe, v)

	case ^FunctionArg:
	//
	case ^Identifier:
		ident_reg, found := qbe_lookup_symbol(qbe, v.value)
		if !found {
			qbe_error(qbe, "undefined identifier: %s", v.value)
			return QbeResult{"", .Invalid}
		}
		reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w loadw %s\n", reg, ident_reg)
		return QbeResult{reg, .Long}

	case ^IntLiteral:
		return QbeResult{fmt.tprintf("%d", v.value), .Word}

	case ^Boolean:
	//
	}
	return QbeResult{"", .Invalid}
}

qbe_gen_func_def :: proc(qbe: ^Qbe, func: ^Function) -> QbeResult {
	qbe_push_scope(qbe)
	qbe.current_func_temp_count = 0
	if func.name.value == "main" {
		qbe_emit(qbe, "export function w $main(")
	} else {
		qbe_emit(qbe, "function w $%s(", func.name.value)
	}
	// TODO: args
	qbe_emit(qbe, ") {{\n")
	qbe_emit(qbe, "@start\n")
	qbe_gen_stmt(qbe, func.body)
	qbe_emit(qbe, "}}\n")
	label := fmt.tprintf("$%s", func.name.value)
	qbe_add_symbol(qbe, func.name.value, label)
	return QbeResult{label, .Long}
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
