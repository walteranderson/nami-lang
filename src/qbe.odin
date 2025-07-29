package nami

import "core:fmt"
import "core:mem"
import os "core:os/os2"
import "core:strings"

import "ast"
import "logger"

Qbe :: struct {
	allocator:        mem.Allocator,
	sb:               strings.Builder,
	program:          ^ast.Program,
	errors:           [dynamic]string,
	strs:             map[string]string,
	str_count:        int,
	symbols:          [dynamic]QbeSymbolTable,
	func_temp_count:  int,
	label_temp_count: int,
}

QbeType :: enum {
	Invalid,
	Void,
	Word, // 32-bit integer
	Long, // 64-bit integer or pointer/address
	Byte, // 8-bit integer
	Half, // 16-bit integer
	Float, // Single-precision float
	Double, // Double-precision float
}

QbeResult :: struct {
	value: string,
	type:  QbeType,
}

SymbolKind :: enum {
	Local,
	Global,
	Func,
	FuncArg,
}

QbeSymbolEntry :: struct {
	name:      string,
	register:  string,
	lang_type: ast.TypeKind,
	qbe_type:  QbeType,
	kind:      SymbolKind,
}

QbeSymbolTable :: map[string]^QbeSymbolEntry

qbe_init :: proc(
	qbe: ^Qbe,
	program: ^ast.Program,
	global_symbols: map[string]^ast.TypeInfo,
	allocator: mem.Allocator,
) {
	qbe.program = program
	qbe.allocator = allocator
	qbe.errors = make([dynamic]string, 0, allocator)
	strings.builder_init(&qbe.sb, allocator)

	qbe.symbols = make([dynamic]QbeSymbolTable, 0, allocator)
	qbe_push_scope(qbe)
	qbe_add_global_symbols(qbe, global_symbols)
}

qbe_add_global_symbols :: proc(qbe: ^Qbe, global_symbols: map[string]^ast.TypeInfo) {
	for key, value in global_symbols {
		reg := qbe_new_temp_reg(qbe)
		qbe_add_symbol(qbe, key, reg, value.kind, .Global)
	}
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

qbe_gen_stmt :: proc(qbe: ^Qbe, stmt: ast.Statement) {
	switch s in stmt {
	case ^ast.FunctionStatement:
		qbe_push_scope(qbe)
		qbe.func_temp_count = 0
		qbe.label_temp_count = 0
		is_main := false
		if s.name.value == "main" {
			is_main = true
			qbe_emit(qbe, "export ")
		}

		func_typeinfo := s.resolved_type.data.(ast.FunctionTypeInfo)

		qbe_emit(qbe, "function ")
		if func_typeinfo.return_type.kind != .Void {
			qbe_emit(
				qbe,
				"%s ",
				qbe_type_to_string(qbe_lang_type_to_qbe_type(func_typeinfo.return_type.kind)),
			)
		}
		qbe_emit(qbe, "$%s(", s.name.value)

		for arg, idx in s.args {
			reg := qbe_new_temp_reg(qbe)
			qbe_add_symbol(qbe, arg.ident.value, reg, arg.resolved_type.kind, .FuncArg)
			qbe_emit(
				qbe,
				"%s %s",
				qbe_type_to_string(qbe_lang_type_to_qbe_type(arg.resolved_type.kind)),
				reg,
			)
			if idx + 1 < len(s.args) {
				qbe_emit(qbe, ", ")
			}
		}

		qbe_emit(qbe, ") {{\n")
		qbe_emit(qbe, "%s\n", qbe_new_temp_label(qbe, "start"))
		has_return := false
		for st in s.body.stmts {
			if _, ok := st.(^ast.ReturnStatement); ok {
				has_return = true
			}
			qbe_gen_stmt(qbe, st)
		}
		if !has_return {
			if is_main {
				qbe_emit(qbe, "  ret 0\n")
			} else {
				qbe_emit(qbe, "  ret\n")
			}
		}
		qbe_emit(qbe, "}}\n")
		qbe_pop_scope(qbe)

		register := fmt.tprintf("$%s", s.name.value)
		qbe_add_symbol(qbe, s.name.value, register, func_typeinfo.return_type.kind, .Func)

	case ^ast.BlockStatement:
		qbe_error(qbe, "Unreachable - block statement")
	case ^ast.FunctionArg:
		qbe_error(qbe, "Unreachable - function arg")

	case ^ast.ReturnStatement:
		if s.value != nil {
			res := qbe_gen_expr(qbe, s.value)
			qbe_emit(qbe, "  ret %s\n", res.value)
		} else {
			qbe_emit(qbe, "  ret\n")
		}

	case ^ast.AssignStatement:
		if len(qbe.symbols) == 1 {
			if s.resolved_type.kind != .String &&
			   s.resolved_type.kind != .Bool &&
			   s.resolved_type.kind != .Int {
				qbe_error(
					qbe,
					"Unsupported constant - global constants can only be string, bool, or int",
				)
				return
			}

			symbol_sb: strings.Builder
			strings.builder_init(&symbol_sb, qbe.allocator)
			defer strings.builder_destroy(&symbol_sb)
			fmt.sbprintf(&symbol_sb, "$%s", s.name.value)

			symbol_register := strings.to_string(symbol_sb)
			qbe_add_symbol(qbe, s.name.value, symbol_register, s.resolved_type.kind, .Global)

			data_type: QbeType
			if s.resolved_type.kind == .String {
				data_type = .Byte
				str := s.value.(^ast.StringLiteral)

				// add null termination
				val_sb: strings.Builder
				strings.builder_init(&val_sb, qbe.allocator)
				defer strings.builder_destroy(&val_sb)
				fmt.sbprintf(&val_sb, "%s\\00", str.value)
				val := strings.to_string(val_sb)

				qbe_emit(
					qbe,
					"data %s = {{ %s \"%s\" }}\n",
					symbol_register,
					qbe_type_to_string(data_type),
					val,
				)
			} else if s.resolved_type.kind == .Bool {
				data_type = qbe_lang_type_to_qbe_type(s.resolved_type.kind)
				bol := s.value.(^ast.Boolean)
				qbe_emit(
					qbe,
					"data %s = {{ %s %d }}\n",
					symbol_register,
					qbe_type_to_string(data_type),
					bol.value,
				)
			} else {
				data_type = qbe_lang_type_to_qbe_type(s.resolved_type.kind)
				lit := s.value.(^ast.IntLiteral)
				qbe_emit(
					qbe,
					"data %s = {{ %s %d }}\n",
					symbol_register,
					qbe_type_to_string(data_type),
					lit.value,
				)
			}
		} else {
			reg_ptr := qbe_new_temp_reg(qbe)
			qbe_add_symbol(qbe, s.name.value, reg_ptr, s.resolved_type.kind, .Local)
			size := qbe_lang_type_to_size(s.resolved_type.kind)
			qbe_emit(qbe, "  %s =l alloc%d %d\n", reg_ptr, size, size)
			if s.value == nil {
				qbe_emit(
					qbe,
					"  store%s %d, %s\n",
					qbe_type_to_string(qbe_lang_type_to_qbe_type(s.resolved_type.kind)),
					0,
					reg_ptr,
				)
			} else {
				res := qbe_gen_expr(qbe, s.value)
				qbe_emit(
					qbe,
					"  store%s %s, %s\n",
					qbe_type_to_string(res.type),
					res.value,
					reg_ptr,
				)
			}
		}
	case ^ast.ReassignStatement:
		entry, found := qbe_lookup_symbol(qbe, s.name.value)
		if !found {
			qbe_error(qbe, "%s is not defined", s.name.value)
			return
		}
		res := qbe_gen_expr(qbe, s.value)
		qbe_emit(
			qbe,
			"  store%s %s, %s\n",
			qbe_type_to_string(res.type),
			res.value,
			entry.register,
		)
	case ^ast.ExprStatement:
		qbe_gen_expr(qbe, s.value)

	case ^ast.Program:
		qbe_error(qbe, "Unexpected program")

	case ^ast.IfStatement:
		qbe_gen_if_stmt(qbe, s)

	case:
		logger.error("QBE generating statement unreachable: %+v", stmt)
	}
}

qbe_gen_if_stmt :: proc(qbe: ^Qbe, stmt: ^ast.IfStatement) {
	result := qbe_gen_expr(qbe, stmt.condition)
	if result.type == .Invalid {
		return
	}

	if_true_label := qbe_new_temp_label(qbe, "if_true")
	if_false_label := qbe_new_temp_label(qbe, "if_false")

	qbe_emit(qbe, "  jnz %s, %s, %s\n", result.value, if_true_label, if_false_label)

	qbe_emit(qbe, "%s\n", if_true_label)
	for s in stmt.consequence.stmts {
		qbe_gen_stmt(qbe, s)
	}

	join_label: string
	if stmt.alternative != nil {
		join_label = qbe_new_temp_label(qbe, "if_join")
		qbe_emit(qbe, "  jmp %s\n", join_label)
	}
	qbe_emit(qbe, "%s\n", if_false_label)

	if stmt.alternative != nil {
		for s in stmt.alternative.stmts {
			qbe_gen_stmt(qbe, s)
		}
		qbe_emit(qbe, "%s\n", join_label)
	}
}

qbe_gen_expr :: proc(qbe: ^Qbe, expr: ast.Expr) -> QbeResult {
	switch v in expr {
	case ^ast.InfixExpr:
		lhs := qbe_gen_expr(qbe, v.left)
		rhs := qbe_gen_expr(qbe, v.right)
		if lhs.type == .Invalid || rhs.type == .Invalid {
			return QbeResult{"", .Invalid}
		}

		// TODO: what other types should i support?
		if lhs.type != .Word || rhs.type != .Word {
			qbe_error(qbe, "only integers and booleans are allowed for infix expressions")
			return QbeResult{"", .Invalid}
		}
		res_type: QbeType = .Word

		op_str := ""
		#partial switch v.tok.type {
		case .PLUS:
			op_str = "add"
		case .MINUS:
			op_str = "sub"
		case .STAR:
			op_str = "mul"
		case .SLASH:
			op_str = "div"
		case .EQ:
			op_str = strings.concatenate(
				[]string{"ceq", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case .NOT_EQ:
			op_str = strings.concatenate(
				[]string{"cne", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case .GT:
			op_str = strings.concatenate(
				[]string{"csgt", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case .LT:
			op_str = strings.concatenate(
				[]string{"cslt", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case:
			qbe_error(qbe, "Unsupported operator: %s", v.op)
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

	case ^ast.PrefixExpr:
		switch v.op {
		case "-":
			rhs := qbe_gen_expr(qbe, v.right)
			reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s copy %s\n", reg, qbe_type_to_string(rhs.type), rhs.value)
			neg_reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s neg %s\n", neg_reg, qbe_type_to_string(rhs.type), reg)
			return QbeResult{neg_reg, rhs.type}
		case "!":
			rhs := qbe_gen_expr(qbe, v.right)
			reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s copy %s\n", reg, qbe_type_to_string(rhs.type), rhs.value)
			not_reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s xor %s, 1\n", not_reg, qbe_type_to_string(rhs.type), reg)
			return QbeResult{not_reg, rhs.type}
		case:
			qbe_error(qbe, "Unsupported prefix operator: %s", v.op)
			return QbeResult{"", .Invalid}
		}

	case ^ast.CallExpr:
		return_type: QbeType
		// TODO: better builtin handling
		// TODO: (variadic support) currently hardcoding only for printf
		is_variadic := false
		if v.func.value == "printf" {
			return_type = .Word
			is_variadic = true
		} else {
			entry, found := qbe_lookup_symbol(qbe, v.func.value)
			if !found {
				qbe_error(qbe, "Identifier not found %s", v.func.value)
				return QbeResult{"", .Invalid}
			}
			return_type = entry.qbe_type
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

		sb: strings.Builder
		strings.builder_init(&sb, qbe.allocator)
		defer strings.builder_destroy(&sb)
		fmt.sbprintf(&sb, "call $%s(", v.func.value)
		for arg, i in args_reg {
			fmt.sbprintf(&sb, "%s %s", qbe_type_to_string(arg.type), arg.value)
			if i + 1 < len(args_reg) {
				fmt.sbprintf(&sb, ", ")
				if i == 0 && is_variadic {
					fmt.sbprintf(&sb, " ..., ")
				}
			}
		}
		fmt.sbprintf(&sb, ")\n")

		if return_type == .Void {
			qbe_emit(qbe, "  %s", strings.to_string(sb))
			return QbeResult{"", .Void}
		}

		ret_reg := qbe_new_temp_reg(qbe)
		qbe_emit(
			qbe,
			"  %s =%s %s",
			ret_reg,
			qbe_type_to_string(return_type),
			strings.to_string(sb),
		)
		return QbeResult{ret_reg, return_type}

	case ^ast.StringLiteral:
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
	case ^ast.Identifier:
		ident, found := qbe_lookup_symbol(qbe, v.value)
		if !found {
			qbe_error(qbe, "undefined identifier: %s", v.value)
			return QbeResult{"", .Invalid}
		}
		switch ident.kind {
		case .Local:
			reg := qbe_new_temp_reg(qbe)
			type := qbe_type_to_string(ident.qbe_type)
			qbe_emit(qbe, "  %s =%s load%s %s\n", reg, type, type, ident.register)
			return QbeResult{reg, ident.qbe_type}
		case .FuncArg:
			return QbeResult{ident.register, ident.qbe_type}
		case .Func:
		// TODO: function identifier, do i want to support this?
		case .Global:
			if ident.qbe_type == .Long {
				return QbeResult{ident.register, ident.qbe_type}
			}
			// if its not a pointer (long) then its a word and we need to load it into a temp register
			reg := qbe_new_temp_reg(qbe)
			type := qbe_type_to_string(ident.qbe_type)
			qbe_emit(qbe, "  %s =%s load%s %s\n", reg, type, type, ident.register)
			return QbeResult{reg, ident.qbe_type}
		}

	case ^ast.IntLiteral:
		return QbeResult{fmt.tprintf("%d", v.value), .Word}

	case ^ast.Boolean:
		return QbeResult{v.value ? "1" : "0", .Word}

	}
	return QbeResult{"", .Invalid}
}

qbe_new_temp_reg :: proc(qbe: ^Qbe) -> string {
	qbe.func_temp_count += 1
	name := fmt.tprintf("%%.%d", qbe.func_temp_count)
	return name
}

qbe_new_temp_label :: proc(qbe: ^Qbe, name: string) -> string {
	qbe.label_temp_count += 1
	label := fmt.tprintf("@%s.%d", name, qbe.label_temp_count)
	return label
}

qbe_emit :: proc(qbe: ^Qbe, format: string, args: ..any) {
	fmt.sbprintf(&qbe.sb, format, ..args)
}

qbe_error :: proc(qbe: ^Qbe, ft: string, args: ..any) {
	append(&qbe.errors, fmt.tprintf(ft, ..args))
}

qbe_push_scope :: proc(qbe: ^Qbe) {
	scope := make(map[string]^QbeSymbolEntry, qbe.allocator)
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

qbe_add_symbol :: proc(
	qbe: ^Qbe,
	name: string,
	register: string,
	type: ast.TypeKind,
	kind: SymbolKind,
) {
	if len(qbe.symbols) <= 0 {
		qbe_error(qbe, "can't add symbols to an empty stack")
		return
	}
	entry := new(QbeSymbolEntry, qbe.allocator)
	entry.name = strings.clone(name, qbe.allocator)
	entry.register = strings.clone(register, qbe.allocator)
	entry.lang_type = type
	entry.qbe_type = qbe_lang_type_to_qbe_type(type)
	entry.kind = kind
	qbe.symbols[len(qbe.symbols) - 1][entry.name] = entry
}

qbe_lookup_symbol :: proc(qbe: ^Qbe, name: string) -> (^QbeSymbolEntry, bool) {
	for i := len(qbe.symbols) - 1; i >= 0; i -= 1 {
		scope := qbe.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

qbe_compile :: proc(qbe: ^Qbe, program_name: string) -> (err: os.Error) {
	qbe_file := create_file_name(program_name, "ssa")
	asm_file := create_file_name(program_name, "s")

	content := strings.to_string(qbe.sb)
	os.write_entire_file(qbe_file, transmute([]byte)(content)) or_return

	qbe_cmd := []string{"qbe", "-o", asm_file, qbe_file}
	logger.info("Running command: %v", qbe_cmd)

	qbe_desc := os.Process_Desc {
		command = qbe_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(qbe_desc) or_return

	cc_cmd := []string{"cc", "-o", program_name, asm_file}
	logger.info("Running command: %v", cc_cmd)

	cc_desc := os.Process_Desc {
		command = cc_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(cc_desc) or_return
	return nil
}

qbe_lang_type_to_size :: proc(type: ast.TypeKind) -> int {
	switch type {
	case .String:
		return 8
	case .Int:
		return 4
	case .Bool:
		return 4
	case .Any:
		// TODO: Any type - assuming that if we get an `any` at this point, maybe its a pointer?
		return 8
	case .Function:
		// TODO: Kind of assuming that maybe in the future, function size will refer to a function pointer
		return 8
	case .Void:
		return 0
	case .Invalid:
		return 0
	}
	return 0
}

qbe_lang_type_to_qbe_type :: proc(type: ast.TypeKind) -> QbeType {
	switch type {
	case .String:
		return .Long
	case .Int:
		return .Word
	case .Bool:
		return .Word
	case .Any:
		// TODO: how to handle `any` types?
		return .Long
	case .Function:
		return .Long
	case .Void:
		return .Void
	case .Invalid:
		return .Invalid
	}
	return .Invalid
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
	case .Void:
		return ""
	case .Invalid:
		return ""
	}
	return ""
}
