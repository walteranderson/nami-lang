package codegen

import "core:fmt"
import "core:mem"
import "core:strings"

import "../ast"
import "../logger"

Qbe :: struct {
	allocator:        mem.Allocator,
	sb:               strings.Builder,
	module:           ^ast.Module,
	errors:           [dynamic]string,
	strs:             map[string]string,
	str_count:        int,
	symbols:          [dynamic]QbeSymbolTable,
	return_stack:     [dynamic]^ReturnContext,
	loop_end_labels:  [dynamic]string,
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
	value:    string,
	type:     QbeType,
	is_array: bool,
}

SymbolKind :: enum {
	Local,
	Global,
	Func,
	FuncArg,
}

QbeSymbolEntry :: struct {
	name:     string,
	register: string,
	typeinfo: ^ast.TypeInfo,
	qbe_type: QbeType,
	kind:     SymbolKind,
}

QbeSymbolTable :: map[string]^QbeSymbolEntry

ReturnContext :: struct {
	type:         ^ast.TypeInfo,
	has_returned: bool,
}

qbe_init :: proc(
	qbe: ^Qbe,
	module: ^ast.Module,
	global_symbols: map[string]^ast.TypeInfo,
	allocator: mem.Allocator,
) {
	qbe.module = module
	qbe.allocator = allocator
	qbe.errors = make([dynamic]string, 0, allocator)
	strings.builder_init(&qbe.sb, allocator)

	qbe.symbols = make([dynamic]QbeSymbolTable, 0, allocator)
	qbe.return_stack = make([dynamic]^ReturnContext, allocator)
	qbe.loop_end_labels = make([dynamic]string, allocator)
	qbe_push_symbol_scope(qbe)
	qbe_add_global_symbols(qbe, global_symbols)
}

qbe_add_global_symbols :: proc(qbe: ^Qbe, global_symbols: map[string]^ast.TypeInfo) {
	for key, value in global_symbols {
		if value.kind == .Function {
			typeinfo := value.data.(ast.FunctionTypeInfo)
			register := fmt.tprintf("$%s", key)
			qbe_add_symbol(qbe, key, register, typeinfo.return_type, .Global)
		} else {
			reg := qbe_new_temp_reg(qbe)
			qbe_add_symbol(qbe, key, reg, value, .Global)
		}
	}
}

qbe_generate :: proc(qbe: ^Qbe) {
	for stmt in qbe.module.stmts {
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
		func_typeinfo := s.resolved_type.data.(ast.FunctionTypeInfo)
		qbe_push_symbol_scope(qbe)
		qbe_push_return_stack(qbe, func_typeinfo.return_type)
		qbe.func_temp_count = 0
		qbe.label_temp_count = 0
		is_main := false
		if s.name.value == "main" {
			is_main = true
			qbe_emit(qbe, "export ")
		}


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
			qbe_add_symbol(qbe, arg.ident.value, reg, arg.resolved_type, .FuncArg)
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
		qbe_gen_stmt(qbe, s.body)

		return_ctx := qbe_get_top_of_return_stack(qbe)
		if !return_ctx.has_returned {
			if is_main {
				qbe_emit(qbe, "  ret 0\n")
			} else if return_ctx.type.kind == .Void {
				qbe_emit(qbe, "  ret\n")
			} else {
				qbe_error(
					qbe,
					"Non-void function does not return a value, return type: %s",
					return_ctx.type.kind,
				)
			}
		}

		qbe_emit(qbe, "}}\n")
		qbe_pop_symbol_scope(qbe)
		qbe_pop_return_stack(qbe)

		_, exists := qbe_lookup_symbol(qbe, s.name.value)
		if !exists {
			register := fmt.tprintf("$%s", s.name.value)
			qbe_add_symbol(qbe, s.name.value, register, func_typeinfo.return_type, .Func)
		}

	case ^ast.BlockStatement:
		for bs in s.stmts {
			qbe_gen_stmt(qbe, bs)
		}

	case ^ast.FunctionArg:
		// function args are processed as part of FunctionStatement
		qbe_error(qbe, "Unreachable - function arg")

	case ^ast.ReturnStatement:
		if len(qbe.return_stack) <= 0 {
			qbe_error(qbe, "Return statement found outside a function")
			return
		}
		qbe_get_top_of_return_stack(qbe).has_returned = true
		if s.value != nil {
			res := qbe_gen_expr(qbe, s.value)
			qbe_emit(qbe, "  ret %s\n", res.value)
		} else {
			qbe_emit(qbe, "  ret\n")
		}

	case ^ast.AssignStatement:
		qbe_gen_assign_stmt(qbe, s)

	case ^ast.ExprStatement:
		qbe_gen_expr(qbe, s.value)

	case ^ast.Module:
		qbe_error(qbe, "Unexpected module")

	case ^ast.IfStatement:
		qbe_gen_if_stmt(qbe, s)

	case ^ast.LoopStatement:
		qbe_gen_loop_stmt(qbe, s)

	case ^ast.BreakStatement:
		if len(qbe.loop_end_labels) <= 0 {
			qbe_error(qbe, "Break statement found outside of a loop")
			return
		}
		current_loop_end_label := qbe.loop_end_labels[len(qbe.loop_end_labels) - 1]
		qbe_emit(qbe, "  jmp %s\n", current_loop_end_label)

	case:
		logger.error("QBE generating statement unreachable: %+v", stmt)
	}
}

qbe_gen_assign_stmt :: proc(qbe: ^Qbe, s: ^ast.AssignStatement) {
	if len(qbe.symbols) == 1 {
		// Global Assignments

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
		qbe_add_symbol(qbe, s.name.value, symbol_register, s.resolved_type, .Global)

		if s.value == nil {
			qbe_emit(
				qbe,
				"data %s = {{ z %d }}\n",
				symbol_register,
				qbe_lang_type_to_size(s.resolved_type.kind),
			)
		} else {
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
			} else if s.resolved_type.kind == .Int {
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
		}
	} else {
		// Local-scoped Assignments

		if s.value == nil {
			if s.resolved_type.kind == .Array {
				reg := qbe_new_temp_reg(qbe)
				qbe_gen_empty_array(qbe, s.resolved_type, reg)
			} else {
				reg := qbe_new_temp_reg(qbe)
				size := qbe_lang_type_to_size(s.resolved_type.kind)
				qbe_emit(qbe, "  %s =l alloc%d %d\n", reg, size, size)
				qbe_emit(
					qbe,
					"  store%s %d, %s\n",
					qbe_type_to_string(qbe_lang_type_to_qbe_type(s.resolved_type.kind)),
					0,
					reg,
				)
				qbe_add_symbol(qbe, s.name.value, reg, s.resolved_type, .Local)
			}
		} else {
			res := qbe_gen_expr(qbe, s.value)
			if res.is_array {
				qbe_add_symbol(qbe, s.name.value, res.value, s.resolved_type, .Local)
			} else {
				reg := qbe_new_temp_reg(qbe)
				size := qbe_lang_type_to_size(s.resolved_type.kind)
				qbe_emit(qbe, "  %s =l alloc%d %d\n", reg, size, size)
				qbe_emit(qbe, "  store%s %s, %s\n", qbe_type_to_string(res.type), res.value, reg)
				qbe_add_symbol(qbe, s.name.value, reg, s.resolved_type, .Local)
			}
		}
	}
}

qbe_gen_loop_stmt :: proc(qbe: ^Qbe, loop: ^ast.LoopStatement) {
	loop_begin := qbe_new_temp_label(qbe, "loop_begin")
	loop_end := qbe_new_temp_label(qbe, "loop_end")


	switch loop.kind {
	case .Infinite:
		qbe_emit(qbe, "%s\n", loop_begin)
		qbe_push_loop_end_label(qbe, loop_end)

		qbe_gen_stmt(qbe, loop.block)

		qbe_emit(qbe, "  jmp %s\n", loop_begin)
		qbe_emit(qbe, "%s\n", loop_end)
		qbe_pop_loop_end_label(qbe)
	case .When:
		qbe_emit(qbe, "%s\n", loop_begin)
		qbe_push_loop_end_label(qbe, loop_end)

		// when condition check
		loop_body := qbe_new_temp_label(qbe, "loop_body")
		result := qbe_gen_expr(qbe, loop.wehn)
		qbe_emit(qbe, "  jnz %s, %s, %s\n", result.value, loop_body, loop_end)

		// loop body
		qbe_emit(qbe, "%s\n", loop_body)
		qbe_gen_stmt(qbe, loop.block)
		qbe_emit(qbe, "  jmp %s\n", loop_begin)

		// end
		qbe_emit(qbe, "%s\n", loop_end)
		qbe_pop_loop_end_label(qbe)
	case .Iterator:
		qbe_emit(qbe, "%s\n", loop_begin)
		qbe_push_loop_end_label(qbe, loop_end)

		idx_reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =l alloc4 4\n", idx_reg)
		qbe_emit(qbe, "  storew 0, %s\n", idx_reg)

		if loop.idx != nil {
			idx_ident, ok := loop.idx.(^ast.Identifier)
			if !ok {
				qbe_error(qbe, "expected index to be identifier")
				return
			}
			qbe_add_symbol(qbe, idx_ident.value, idx_reg, idx_ident.resolved_type, .Local)
		}

		item_ident, okk := loop.item.(^ast.Identifier)
		if !okk {
			qbe_error(qbe, "expected item to be identifier")
			return
		}

		items_ident, okkk := loop.items.(^ast.Identifier)
		if !okk {
			qbe_error(qbe, "expected items to be identifier")
			return
		}

		items_symbol, ok := qbe_lookup_symbol(qbe, items_ident.value)
		if !ok {
			qbe_error(qbe, "Identifier not found %s", items_ident.value)
			return
		}
		array_element_typeinfo, okkkk := items_symbol.typeinfo.data.(ast.ArrayTypeInfo)
		if !okkkk {
			qbe_error(qbe, "expected array to have typeinfo")
			return
		}

		loop_cond := qbe_new_temp_label(qbe, "loop_cond")

		qbe_emit(qbe, "  jmp %s\n", loop_cond)

		qbe_emit(qbe, "%s\n", loop_cond)

		loop_body := qbe_new_temp_label(qbe, "loop_body")
		// because I only support fixed arrays, I'm going to hard-code the array length in the conditional.
		// Once I decide to support dynamic arrays, this may need to change.
		cond_reg := qbe_new_temp_reg(qbe)
		idx_val_reg1 := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w loadw %s\n", idx_val_reg1, idx_reg)
		qbe_emit(
			qbe,
			"  %s =w csltw %s, %d\n",
			cond_reg,
			idx_val_reg1,
			array_element_typeinfo.size,
		)
		qbe_emit(qbe, "  jnz %s, %s, %s\n", cond_reg, loop_body, loop_end)

		qbe_emit(qbe, "%s\n", loop_body)

		offset_reg := qbe_new_temp_reg(qbe)
		element_size := qbe_lang_type_to_size(array_element_typeinfo.elements_type.kind)
		idx_val_reg2 := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w loadw %s\n", idx_val_reg2, idx_reg)
		qbe_emit(qbe, "  %s =w mul %s, %d\n", offset_reg, idx_val_reg2, element_size)

		offset_cast_reg := qbe_new_temp_reg(qbe)
		qbe_emit(
			qbe,
			"  %s =%s extsw %s\n",
			offset_cast_reg,
			qbe_type_to_string(items_symbol.qbe_type),
			offset_reg,
		)

		item_addr := qbe_new_temp_reg(qbe)
		qbe_emit(
			qbe,
			"  %s =%s add %s, %s\n",
			item_addr,
			qbe_type_to_string(items_symbol.qbe_type),
			items_symbol.register,
			offset_cast_reg,
		)
		qbe_add_symbol(qbe, item_ident.value, item_addr, item_ident.resolved_type, .Local)

		qbe_gen_stmt(qbe, loop.block)

		idx_val2_reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w loadw %s\n", idx_val2_reg, idx_reg)

		idx_next_reg := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =w add %s, 1\n", idx_next_reg, idx_val2_reg)

		qbe_emit(qbe, "  storew %s, %s\n", idx_next_reg, idx_reg)

		qbe_emit(qbe, "  jmp %s\n", loop_cond)

		qbe_emit(qbe, "%s\n", loop_end)
		qbe_pop_loop_end_label(qbe)
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

	qbe_push_return_stack(qbe, qbe_get_top_of_return_stack(qbe).type)
	qbe_gen_stmt(qbe, stmt.consequence)

	join_label: string
	if stmt.alternative != nil && !qbe_get_top_of_return_stack(qbe).has_returned {
		join_label = qbe_new_temp_label(qbe, "if_join")
		qbe_emit(qbe, "  jmp %s\n", join_label)
	}
	qbe_pop_return_stack(qbe)

	qbe_emit(qbe, "%s\n", if_false_label)

	if stmt.alternative != nil {
		qbe_gen_stmt(qbe, stmt.alternative)
		if join_label != "" {
			qbe_emit(qbe, "%s\n", join_label)
		}
	}
}

qbe_gen_expr :: proc(qbe: ^Qbe, expr: ast.Expr) -> QbeResult {
	switch v in expr {
	case ^ast.InfixExpr:
		lhs := qbe_gen_expr(qbe, v.left)
		rhs := qbe_gen_expr(qbe, v.right)
		if lhs.type == .Invalid || rhs.type == .Invalid {
			return qbe_make_result("", .Invalid)
		}

		// TODO: what other types should i support?
		if lhs.type != .Word || rhs.type != .Word {
			qbe_error(qbe, "only integers and booleans are allowed for infix expressions")
			return qbe_make_result("", .Invalid)
		}
		res_type: QbeType = .Word

		op_str := ""
		#partial switch v.tok.type {
		case .AND, .OR:
			return qbe_gen_logical_operators(qbe, v)
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
		case .GTE:
			op_str = strings.concatenate(
				[]string{"csge", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case .LT:
			op_str = strings.concatenate(
				[]string{"cslt", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case .LTE:
			op_str = strings.concatenate(
				[]string{"csle", qbe_type_to_string(res_type)},
				qbe.allocator,
			)
		case:
			qbe_error(qbe, "Unsupported operator: %s", v.op)
			return qbe_make_result("", .Invalid)
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
		return qbe_make_result(reg, res_type)

	case ^ast.PrefixExpr:
		switch v.op {
		case "-":
			rhs := qbe_gen_expr(qbe, v.right)
			reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s copy %s\n", reg, qbe_type_to_string(rhs.type), rhs.value)
			neg_reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s neg %s\n", neg_reg, qbe_type_to_string(rhs.type), reg)
			return qbe_make_result(neg_reg, rhs.type)
		case "!":
			rhs := qbe_gen_expr(qbe, v.right)
			reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s copy %s\n", reg, qbe_type_to_string(rhs.type), rhs.value)
			not_reg := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =%s xor %s, 1\n", not_reg, qbe_type_to_string(rhs.type), reg)
			return qbe_make_result(not_reg, rhs.type)
		case:
			qbe_error(qbe, "Unsupported prefix operator: %s", v.op)
			return qbe_make_result("", .Invalid)
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
				return qbe_make_result("", .Invalid)
			}
			return_type = entry.qbe_type
		}

		args_reg := make([dynamic]QbeResult, qbe.allocator)
		defer delete(args_reg)
		for arg in v.args {
			reg := qbe_gen_expr(qbe, arg)
			if reg.type == .Invalid {
				return qbe_make_result("", .Invalid)
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
			return qbe_make_result("", .Void)
		}

		ret_reg := qbe_new_temp_reg(qbe)
		qbe_emit(
			qbe,
			"  %s =%s %s",
			ret_reg,
			qbe_type_to_string(return_type),
			strings.to_string(sb),
		)
		return qbe_make_result(ret_reg, return_type)

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
		return qbe_make_result(label, .Long)
	case ^ast.Identifier:
		ident, found := qbe_lookup_symbol(qbe, v.value)
		if !found {
			qbe_error(qbe, "undefined identifier: %s", v.value)
			return qbe_make_result("", .Invalid)
		}
		switch ident.kind {
		case .Local:
			reg := qbe_new_temp_reg(qbe)
			type := qbe_type_to_string(ident.qbe_type)
			qbe_emit(qbe, "  %s =%s load%s %s\n", reg, type, type, ident.register)
			return qbe_make_result(reg, ident.qbe_type)
		case .FuncArg:
			return qbe_make_result(ident.register, ident.qbe_type)
		case .Func:
		// TODO: function identifier, do i want to support this?
		case .Global:
			if ident.qbe_type == .Long {
				return qbe_make_result(ident.register, ident.qbe_type)
			}
			// if its not a pointer (long) then its a word and we need to load it into a temp register
			reg := qbe_new_temp_reg(qbe)
			type := qbe_type_to_string(ident.qbe_type)
			qbe_emit(qbe, "  %s =%s load%s %s\n", reg, type, type, ident.register)
			return qbe_make_result(reg, ident.qbe_type)
		}

	case ^ast.IntLiteral:
		return qbe_make_result(fmt.tprintf("%d", v.value), .Word)

	case ^ast.Boolean:
		return qbe_make_result(v.value ? "1" : "0", .Word)

	case ^ast.Array:
		return qbe_gen_array_expr(qbe, v)

	case ^ast.IndexExpr:
		return qbe_gen_index_expr(qbe, v)

	case ^ast.ReassignExpr:
		return qbe_gen_reassign_expr(qbe, v)
	}
	return qbe_make_result("", .Invalid)
}

qbe_gen_reassign_expr :: proc(qbe: ^Qbe, expr: ^ast.ReassignExpr) -> QbeResult {
	addr := qbe_gen_lvalue_addr(qbe, expr.target)
	if addr.type == .Invalid {
		logger.error("invalid lvalue")
		return addr
	}

	value_res := qbe_gen_expr(qbe, expr.value)
	qbe_emit(
		qbe,
		"  store%s %s, %s\n",
		qbe_type_to_string(value_res.type),
		value_res.value,
		addr.value,
	)
	return qbe_make_result("", .Void)
}

qbe_gen_lvalue_addr :: proc(qbe: ^Qbe, target: ast.Expr) -> QbeResult {
	#partial switch v in target {
	case ^ast.Identifier:
		entry, found := qbe_lookup_symbol(qbe, v.value)
		if !found {
			qbe_error(qbe, "%s is not defined", v.value)
			return qbe_make_result("", .Invalid)
		}
		return qbe_make_result(entry.register, .Long)
	case ^ast.IndexExpr:
		return qbe_gen_index_addr(qbe, v)
	}
	qbe_error(qbe, "%s is not a valid lvalue", ast.get_token_from_expr(target).type)
	return qbe_make_result("", .Invalid)
}

qbe_gen_index_addr :: proc(qbe: ^Qbe, expr: ^ast.IndexExpr) -> QbeResult {
	ident, ok := expr.left.(^ast.Identifier)
	if !ok {
		qbe_error(
			qbe,
			"Expected identifier when using index expression, got %s",
			ast.get_token_from_expr(expr.left).type,
		)
		return qbe_make_result("", .Invalid)
	}
	symbol, okk := qbe_lookup_symbol(qbe, ident.value)
	if !okk {
		qbe_error(qbe, "Identifier not found: %s", ident.value)
		return qbe_make_result("", .Invalid)
	}
	if symbol.typeinfo.kind != .Array {
		qbe_error(qbe, "Expected array, got %s", symbol.typeinfo.kind)
		return qbe_make_result("", .Invalid)
	}
	idx, okkk := expr.index.(^ast.IntLiteral)
	if !okkk {
		// TODO
		// NOTE: For non-literal indices (e.g., 'a[i+1]'), you would 
		// need to call qbe_gen_expr on expr.index to get the runtime index value
		qbe_error(
			qbe,
			"Expected index to be integer literal, got %s",
			ast.get_token_from_expr(expr.index).type,
		)
		return qbe_make_result("", .Invalid)
	}

	typeinfo := symbol.typeinfo.data.(ast.ArrayTypeInfo)
	element_size := qbe_lang_type_to_size(typeinfo.elements_type.kind)
	base_reg := symbol.register

	if idx.value == 0 {
		return qbe_make_result(base_reg, .Long)
	}

	add_reg := qbe_new_temp_reg(qbe)
	size := element_size * idx.value
	qbe_emit(
		qbe,
		"  %s =%s add %s, %d\n",
		add_reg,
		qbe_type_to_string(symbol.qbe_type),
		base_reg,
		size,
	)
	return qbe_make_result(add_reg, .Long)
}


qbe_gen_index_expr :: proc(qbe: ^Qbe, expr: ^ast.IndexExpr) -> QbeResult {
	addr := qbe_gen_index_addr(qbe, expr)
	if addr.type == .Invalid {
		return addr
	}

	element_qbe_type := qbe_lang_type_to_qbe_type(expr.resolved_type.kind)
	type_str := qbe_type_to_string(element_qbe_type)

	reg := qbe_new_temp_reg(qbe)
	qbe_emit(qbe, "  %s =%s load%s %s\n", reg, type_str, type_str, addr.value)
	return qbe_make_result(reg, element_qbe_type)
}

qbe_gen_array_expr :: proc(qbe: ^Qbe, v: ^ast.Array) -> QbeResult {
	reg_ptr := qbe_new_temp_reg(qbe)

	if len(v.elements) == 0 {
		qbe_gen_empty_array(qbe, v.resolved_type, reg_ptr)
	} else {
		typeinfo := v.resolved_type.data.(ast.ArrayTypeInfo)
		type_str := qbe_type_to_string(qbe_lang_type_to_qbe_type(typeinfo.elements_type.kind))
		size := qbe_lang_type_to_size(typeinfo.elements_type.kind)
		capacity := typeinfo.size * size
		qbe_emit(qbe, "  %s =l alloc%d %d\n", reg_ptr, size, capacity)

		first_res := qbe_gen_expr(qbe, v.elements[0])
		qbe_emit(qbe, "  store%s %s, %s\n", type_str, first_res.value, reg_ptr)
		offset := size
		i := 1
		for offset < capacity {
			res_value: string
			if i < len(v.elements) {
				res_value = qbe_gen_expr(qbe, v.elements[i]).value
			} else {
				res_value = "0"
			}
			ptr := qbe_new_temp_reg(qbe)
			qbe_emit(qbe, "  %s =l add %s, %d\n", ptr, reg_ptr, offset)
			qbe_emit(qbe, "  store%s %s, %s\n", type_str, res_value, ptr)
			offset = offset + size
			i = i + 1
		}
	}

	return qbe_make_result(reg_ptr, .Long, true)
}

qbe_gen_empty_array :: proc(qbe: ^Qbe, typeinfo: ^ast.TypeInfo, reg: string) {
	data, ok := typeinfo.data.(ast.ArrayTypeInfo)
	if !ok {
		qbe_error(qbe, "Expected array typeinfo, got %s", typeinfo.kind)
		return
	}

	size := qbe_lang_type_to_size(data.elements_type.kind)
	type_str := qbe_type_to_string(qbe_lang_type_to_qbe_type(data.elements_type.kind))
	capacity := data.size * size
	qbe_emit(qbe, "  %s =l alloc%d %d\n", reg, size, capacity)
	qbe_emit(qbe, "  store%s %d, %s\n", type_str, 0, reg)
	offset := size
	for offset < capacity {
		ptr := qbe_new_temp_reg(qbe)
		qbe_emit(qbe, "  %s =l add %s, %d\n", ptr, reg, offset)
		qbe_emit(qbe, "  store%s %d, %s\n", type_str, 0, ptr)
		offset = offset + size
	}
}

qbe_gen_logical_operators :: proc(qbe: ^Qbe, expr: ^ast.InfixExpr) -> QbeResult {
	logic_right := qbe_new_temp_label(qbe, "logic_right")
	true_merge := qbe_new_temp_label(qbe, "true_merge")
	false_merge := qbe_new_temp_label(qbe, "false_merge")
	assign_merge := qbe_new_temp_label(qbe, "assign_merge")

	left_result := qbe_gen_expr(qbe, expr.left)
	if expr.tok.type == .AND {
		qbe_emit(qbe, "  jnz %s, %s, %s\n", left_result.value, logic_right, false_merge)
	} else {
		qbe_emit(qbe, "  jnz %s, %s, %s\n", left_result.value, true_merge, logic_right)
	}

	qbe_emit(qbe, "%s\n", logic_right)
	right_result := qbe_gen_expr(qbe, expr.right)
	qbe_emit(qbe, "  jnz %s, %s, %s\n", right_result.value, true_merge, false_merge)

	qbe_emit(qbe, "%s\n", true_merge)
	qbe_emit(qbe, "  jmp %s\n", assign_merge)
	qbe_emit(qbe, "%s\n", false_merge)
	qbe_emit(qbe, "  jmp %s\n", assign_merge)

	qbe_emit(qbe, "%s\n", assign_merge)

	result_reg := qbe_new_temp_reg(qbe)
	qbe_type := qbe_lang_type_to_qbe_type(expr.resolved_type.kind)

	qbe_emit(
		qbe,
		"  %s =%s phi %s 1, %s 0\n",
		result_reg,
		qbe_type_to_string(qbe_type),
		true_merge,
		false_merge,
	)

	return qbe_make_result(result_reg, qbe_type)
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

qbe_push_symbol_scope :: proc(qbe: ^Qbe) {
	scope := make(map[string]^QbeSymbolEntry, qbe.allocator)
	append(&qbe.symbols, scope)
}

qbe_pop_symbol_scope :: proc(qbe: ^Qbe) {
	if len(qbe.symbols) <= 0 {
		qbe_error(qbe, "attempting to pop scope from an empty symbol stack")
		return
	}
	popped := pop(&qbe.symbols)
	delete(popped)
}

qbe_push_return_stack :: proc(qbe: ^Qbe, return_type: ^ast.TypeInfo) {
	ctx := new(ReturnContext, qbe.allocator)
	ctx.type = return_type
	append(&qbe.return_stack, ctx)
}

qbe_pop_return_stack :: proc(qbe: ^Qbe) {
	if len(qbe.return_stack) <= 0 {
		qbe_error(qbe, "attempting to pop scope from an empty return stack")
		return
	}
	pop(&qbe.return_stack)
}

qbe_get_top_of_return_stack :: proc(qbe: ^Qbe) -> ^ReturnContext {
	return qbe.return_stack[len(qbe.return_stack) - 1]
}

qbe_push_loop_end_label :: proc(qbe: ^Qbe, label: string) {
	append(&qbe.loop_end_labels, label)
}

qbe_pop_loop_end_label :: proc(qbe: ^Qbe) {
	if len(qbe.loop_end_labels) <= 0 {
		qbe_error(qbe, "attempting to pop scope from an empty loop_end stack")
		return
	}
	pop(&qbe.loop_end_labels)
}

qbe_add_symbol :: proc(
	qbe: ^Qbe,
	name: string,
	register: string,
	typeinfo: ^ast.TypeInfo,
	kind: SymbolKind,
) {
	if len(qbe.symbols) <= 0 {
		qbe_error(qbe, "can't add symbols to an empty stack")
		return
	}
	entry := new(QbeSymbolEntry, qbe.allocator)
	entry.name = strings.clone(name, qbe.allocator)
	entry.register = strings.clone(register, qbe.allocator)
	entry.typeinfo = typeinfo
	entry.qbe_type = qbe_lang_type_to_qbe_type(typeinfo.kind)
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
	case .Array:
		return 8
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
	case .Array:
		return .Long
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

qbe_make_result :: proc(value: string, type: QbeType, is_array := false) -> QbeResult {
	return QbeResult{value, type, is_array}
}
