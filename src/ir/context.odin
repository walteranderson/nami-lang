package ir

import "../ast"
import "../logger"
import "../token"
import "core:fmt"
import "core:mem"
import "core:strings"

USER_MAIN_NAME :: "user_main"
SLICE_TYPE_NAME :: "__Slice"

Context :: struct {
	allocator:       mem.Allocator,
	module:          ^Module,
	errors:          [dynamic]logger.CompilerError,
	str_count:       int,
	symbol_stack:    [dynamic]SymbolTable,
	next_temp_id:    int,
	next_label_id:   int,
	loop_end_labels: [dynamic]string,
}

SymbolTable :: map[string]^SymbolEntry
SymbolEntry :: struct {
	name:     string,
	kind:     SymbolKind,
	typeinfo: ^ast.TypeInfo,
	op:       Operand,
}
SymbolKind :: enum {
	Local,
	Global,
	Func,
	FuncParam,
}

new_context :: proc(
	global_symbols: map[string]^ast.TypeInfo,
	allocator: mem.Allocator,
) -> ^Context {
	ctx := new(Context, allocator)
	ctx.allocator = allocator
	ctx.module = new(Module, allocator)
	ctx.errors = make([dynamic]logger.CompilerError, allocator)
	push_symbol_scope(ctx)
	add_global_symbols(ctx, global_symbols)
	add_builtins(ctx)
	return ctx
}

add_builtins :: proc(ctx: ^Context) {
	{ 	// slice type
		slice_typedef_content := SequentialTypeContent{}
		append(&slice_typedef_content.fields, TypeField{.Long, nil})
		append(&slice_typedef_content.fields, TypeField{.Word, nil})
		slice_typedef := make_typedef(
			ctx,
			name = SLICE_TYPE_NAME,
			content = slice_typedef_content,
		)
		add_typedef(ctx, slice_typedef)
	}
}

add_global_symbols :: proc(
	ctx: ^Context,
	global_symbols: map[string]^ast.TypeInfo,
) {
	for key, value in global_symbols {
		kind: SymbolKind
		#partial switch value.kind {
		case .Function:
			kind = .Func
		case:
			kind = .Global
		}
		push_symbol_entry(
			ctx,
			name = key,
			kind = kind,
			typeinfo = value,
			op = Operand{.GlobalSymbol, key},
		)
	}
}

from_ast :: proc(ctx: ^Context, module: ^ast.Module) {
	gen_main(ctx)
	for stmt in module.stmts {
		gen_stmt(ctx, stmt)
	}
}

gen_main :: proc(ctx: ^Context) {
	def := new(FunctionDef, ctx.allocator)
	def.linkage = .Export
	def.name = "main"
	def.return_type = .Word

	argc := FunctionParam {
		type = .Word,
		op   = Operand{.Temporary, make_temp(ctx)},
	}
	argv := FunctionParam {
		type = .Long,
		op   = Operand{.Temporary, make_temp(ctx)},
	}
	append(&def.params, argc, argv)

	block := make_block(ctx, "start")
	append(&def.blocks, block)
	append(&ctx.module.functions, def)

	// allocate args
	args_operand := gen_slice(ctx, argv.op, argc.op)

	// call USER_MAIN_NAME
	return_operand := Operand{.Temporary, make_temp(ctx)}
	call_inst := make_instruction(
		ctx,
		opcode = .Call,
		src1 = Operand{.GlobalSymbol, USER_MAIN_NAME},
		dest = return_operand,
		dest_type = .Word,
	)
	append(
		&call_inst.call_args,
		CallArgument {
			kind = .Regular,
			type = .Aggregate,
			value = args_operand,
			aggregate_name = SLICE_TYPE_NAME,
		},
	)
	add_instruction(ctx, call_inst)

	ret := make_jump(ctx, .Ret)
	ret.data = RetData{return_operand}
	get_last_block(ctx).terminator = ret
}

gen_slice :: proc(ctx: ^Context, head: Operand, length: Operand) -> Operand {
	op := Operand{.Temporary, make_temp(ctx)}

	// allocate slice struct
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Alloc,
			dest = op,
			dest_type = .Long,
			alignment = 8,
			src1 = Operand{.Integer, 16},
		),
	)

	// store head of array
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Store,
			opcode_type = .Long,
			src1 = head,
			src2 = op,
		),
	)

	// calculate offset
	len_operand := Operand{.Temporary, make_temp(ctx)}
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Add,
			dest = len_operand,
			dest_type = .Long,
			src1 = op,
			src2 = Operand{.Integer, 8},
		),
	)

	// store length
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Store,
			opcode_type = .Word,
			src1 = length,
			src2 = len_operand,
		),
	)

	return op
}

gen_stmt :: proc(ctx: ^Context, stmt: ast.Statement) {
	switch v in stmt {
	case ^ast.Module:
		error(ctx, v.tok, "Unexpected module")
	case ^ast.FunctionStatement:
		gen_function_stmt(ctx, v)
	case ^ast.FunctionArg:
		// function args are processed as part of FunctionStatement
		// TODO: maybe this should be an expression instead?
		error(ctx, v.tok, "Unreachable - function_arg")
	case ^ast.BlockStatement:
		gen_block_stmt(ctx, v)
	case ^ast.ReturnStatement:
		gen_return_stmt(ctx, v)
	case ^ast.ExprStatement:
		gen_expr(ctx, v.value)
	case ^ast.AssignStatement:
		gen_assign_stmt(ctx, v)
	case ^ast.IfStatement:
		gen_if_stmt(ctx, v)
	case ^ast.LoopStatement:
		gen_loop_stmt(ctx, v)
	case ^ast.BreakStatement:
		gen_break_stmt(ctx, v)
	case ^ast.StructStatement:
		gen_struct_stmt(ctx, v)
	case ^ast.StructField:
		// struct fields are processed as part of StructStatement
		// TODO: maybe this should be an expression instead?
		error(ctx, v.tok, "Unreachable - struct_field")
	}
}

gen_struct_stmt :: proc(ctx: ^Context, stmt: ^ast.StructStatement) {
	td_content := SequentialTypeContent{}
	for field in stmt.fields {
		tf := TypeField {
			kind = type_ast_to_ir(field.resolved_type.kind),
		}
		#partial switch field.resolved_type.kind {
		case .Struct:
			ti := field.resolved_type.data.(ast.StructTypeInfo)
			tf.data = ti.name
		case .Slice:
			tf.data = SLICE_TYPE_NAME
		case .Array:
			ti := field.resolved_type.data.(ast.ArrayTypeInfo)
			tf.data = ti.size
		}
		append(&td_content.fields, tf)
	}
	add_typedef(
		ctx,
		make_typedef(ctx, name = stmt.name.value, content = td_content),
	)
}

gen_break_stmt :: proc(ctx: ^Context, stmt: ^ast.BreakStatement) {
	if len(ctx.loop_end_labels) <= 0 {
		error(ctx, stmt.tok, "Break statement found outside of a loop")
		return
	}

	current_loop_end_label := ctx.loop_end_labels[len(ctx.loop_end_labels) - 1]

	jmp := make_jump(ctx, .Jmp)
	jmp.data = JmpData{current_loop_end_label}
	get_last_block(ctx).terminator = jmp
}

gen_infinite_loop :: proc(ctx: ^Context, loop: ^ast.LoopStatement) {
	begin := make_block(ctx, "loop_begin")
	end := make_block(ctx, "loop_end")
	add_block(ctx, begin)
	push_loop_end_label(ctx, end.label)

	gen_stmt(ctx, loop.block)

	jmp := make_jump(ctx, .Jmp)
	jmp.data = JmpData{begin.label}
	get_last_block(ctx).terminator = jmp

	add_block(ctx, end)
	pop_loop_end_label(ctx)
}

gen_when_loop :: proc(ctx: ^Context, loop: ^ast.LoopStatement) {
	begin := make_block(ctx, "loop_begin")
	end := make_block(ctx, "loop_end")
	body := make_block(ctx, "loop_body")

	add_block(ctx, begin)
	push_loop_end_label(ctx, end.label)

	condition := gen_expr(ctx, loop.wehn)
	jnz := make_jump(ctx, .Jnz)
	jnz.data = JnzData {
		condition   = condition,
		true_label  = body.label,
		false_label = end.label,
	}
	get_last_block(ctx).terminator = jnz

	add_block(ctx, body)
	gen_stmt(ctx, loop.block)

	jmp := make_jump(ctx, .Jmp)
	jmp.data = JmpData{begin.label}
	get_last_block(ctx).terminator = jmp

	add_block(ctx, end)
	pop_loop_end_label(ctx)
}


gen_iterator_loop :: proc(ctx: ^Context, loop: ^ast.LoopStatement) {
	loop_begin := make_block(ctx, "loop_begin")
	loop_end := make_block(ctx, "loop_end")
	loop_cond := make_block(ctx, "loop_cond")
	loop_body := make_block(ctx, "loop_body")

	// ----------------------
	// Loop Begin
	// ----------------------

	add_block(ctx, loop_begin)
	push_loop_end_label(ctx, loop_end.label)

	idx_operand := gen_iterator_create_idx(ctx, loop)
	jmp := make_jump(ctx, .Jmp)
	jmp.data = JmpData{loop_cond.label}
	loop_begin.terminator = jmp

	// ----------------------
	// Loop Cond
	// ----------------------

	add_block(ctx, loop_cond)

	item_ident := loop.item.(^ast.Identifier)
	items_ident := loop.items.(^ast.Identifier)
	items_symbol, ok := lookup_symbol(ctx, items_ident.value)
	if !ok {
		error(
			ctx,
			items_ident.tok,
			"Identifier not found %s",
			items_ident.value,
		)
		return
	}


	load_idx := gen_iterator_load_idx(ctx, idx_operand)

	// TODO: eventually make all arrays use the slice aggregate type
	count_operand: Operand
	#partial switch data in items_symbol.typeinfo.data {
	case ast.ArrayTypeInfo:
		count_operand = Operand{.Integer, data.size}
	case ast.SliceTypeInfo:
		count_ptr := Operand{.Temporary, make_temp(ctx)}
		add_instruction(
			ctx,
			make_instruction(
				ctx,
				opcode = .Add,
				dest = count_ptr,
				dest_type = .Long,
				src1 = items_symbol.op,
				src2 = Operand{.Integer, 8},
			),
		)
		count_operand = Operand{.Temporary, make_temp(ctx)}
		add_instruction(
			ctx,
			make_instruction(
				ctx,
				opcode = .Load,
				opcode_type = .Word,
				dest = count_operand,
				dest_type = .Word,
				src1 = count_ptr,
			),
		)
	case:
		error(ctx, items_ident.tok, "Unexpected identifier metadata")
		return
	}

	// Do the comparison: if idx is less than the length of the array
	condition := Operand{.Temporary, make_temp(ctx)}
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			dest = condition,
			dest_type = .Word,
			opcode = .Compare,
			comparison_type = .SignedLess,
			opcode_type = .Word,
			src1 = load_idx,
			src2 = count_operand,
		),
	)

	jnz := make_jump(ctx, .Jnz)
	jnz.data = JnzData {
		condition   = condition,
		true_label  = loop_body.label,
		false_label = loop_end.label,
	}
	loop_cond.terminator = jnz

	// ----------------------
	// Loop Body
	// ----------------------

	add_block(ctx, loop_body)

	gen_iterator_item(ctx, idx_operand, items_symbol, item_ident)
	gen_stmt(ctx, loop.block)
	gen_iterator_increment_idx(ctx, idx_operand)

	jump := make_jump(ctx, .Jmp)
	jump.data = JmpData{loop_cond.label}
	get_last_block(ctx).terminator = jump

	add_block(ctx, loop_end)
	pop_loop_end_label(ctx)
}

gen_iterator_create_idx :: proc(
	ctx: ^Context,
	loop: ^ast.LoopStatement,
) -> Operand {
	idx := Operand{.Temporary, make_temp(ctx)}
	idx_alloc_inst := make_instruction(
		ctx,
		opcode = .Alloc,
		dest = idx,
		dest_type = .Long,
		alignment = 4,
		src1 = Operand{.Integer, 4},
	)
	block := get_last_block(ctx)
	append(&block.instructions, idx_alloc_inst)

	idx_store_inst := make_instruction(
		ctx,
		opcode = .Store,
		opcode_type = .Word,
		src1 = Operand{.Integer, 0},
		src2 = idx,
	)
	append(&block.instructions, idx_store_inst)

	if loop.idx != nil {
		ident, ok := loop.idx.(^ast.Identifier)
		if !ok {
			error(
				ctx,
				ast.get_token_from_expr(loop.idx),
				"expected index to be identifier",
			)
			return invalid_op()
		}
		push_symbol_entry(
			ctx,
			name = ident.value,
			kind = .Local,
			typeinfo = ident.resolved_type,
			op = idx,
		)
	}
	return idx
}

gen_iterator_load_idx :: proc(ctx: ^Context, idx_operand: Operand) -> Operand {
	load_idx := Operand{.Temporary, make_temp(ctx)}
	load_idx_inst := make_instruction(
		ctx,
		opcode = .Load,
		opcode_type = .Word,
		dest = load_idx,
		dest_type = .Word,
		src1 = idx_operand,
	)
	block := get_last_block(ctx)
	append(&block.instructions, load_idx_inst)
	return load_idx
}

gen_iterator_increment_idx :: proc(ctx: ^Context, idx_operand: Operand) {
	block := get_last_block(ctx)
	load_idx := gen_iterator_load_idx(ctx, idx_operand)

	next_operand := Operand{.Temporary, make_temp(ctx)}
	add_inst := make_instruction(
		ctx,
		opcode = .Add,
		dest = next_operand,
		dest_type = .Word,
		src1 = load_idx,
		src2 = Operand{.Integer, 1},
	)
	append(&block.instructions, add_inst)

	store_inst := make_instruction(
		ctx,
		opcode = .Store,
		opcode_type = .Word,
		src1 = next_operand,
		src2 = idx_operand,
	)
	append(&block.instructions, store_inst)
}

gen_iterator_item :: proc(
	ctx: ^Context,
	idx_operand: Operand,
	items_symbol: ^SymbolEntry,
	item_ident: ^ast.Identifier,
) {
	// load the index
	load_idx := gen_iterator_load_idx(ctx, idx_operand)

	// multiply index by the element size to get the array offset
	mul_dest := Operand{.Temporary, make_temp(ctx)}
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Mul,
			dest = mul_dest,
			dest_type = .Word,
			src1 = load_idx,
			src2 = Operand {
				.Integer,
				typeinfo_to_size(item_ident.resolved_type),
			},
		),
	)

	// convert from .Word to .Long so its able to be added to the array pointer
	conv_dest := Operand{.Temporary, make_temp(ctx)}
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Convert,
			conversion_type = .EXT_SIGNED_WORD,
			dest = conv_dest,
			dest_type = .Long,
			src1 = mul_dest,
		),
	)


	arr_ptr: Operand
	if items_symbol.typeinfo.kind == .Slice {
		arr_ptr = Operand{.Temporary, make_temp(ctx)}
		add_instruction(
			ctx,
			make_instruction(
				ctx,
				opcode = .Load,
				opcode_type = .Long,
				dest = arr_ptr,
				dest_type = .Long,
				src1 = items_symbol.op,
			),
		)
	} else {
		arr_ptr = items_symbol.op
	}

	// add to the array pointer
	add_dest := Operand{.Temporary, make_temp(ctx)}
	add_instruction(
		ctx,
		make_instruction(
			ctx,
			opcode = .Add,
			dest = add_dest,
			dest_type = .Long,
			src1 = arr_ptr,
			src2 = conv_dest,
		),
	)

	// add symbol to symbol_table
	push_symbol_entry(
		ctx,
		name = item_ident.value,
		kind = .Local,
		typeinfo = item_ident.resolved_type,
		op = add_dest,
	)
}

gen_loop_stmt :: proc(ctx: ^Context, loop: ^ast.LoopStatement) {
	push_symbol_scope(ctx)
	defer pop_symbol_scope(ctx)

	switch loop.kind {
	case .Infinite:
		gen_infinite_loop(ctx, loop)
	case .When:
		gen_when_loop(ctx, loop)
	case .Iterator:
		gen_iterator_loop(ctx, loop)
	}
}

gen_if_stmt :: proc(ctx: ^Context, stmt: ^ast.IfStatement) {
	if get_last_block(ctx).terminator != nil {
		error(ctx, stmt.tok, "if statement after already declared terminator")
		return
	}
	push_symbol_scope(ctx)
	defer pop_symbol_scope(ctx)

	true_block := make_block(ctx, "if_true")
	false_block := make_block(ctx, "if_false")

	condition := gen_expr(ctx, stmt.condition)
	if condition.kind == .Invalid {
		return
	}

	jump := make_jump(ctx, .Jnz)
	jump.data = JnzData {
		condition   = condition,
		true_label  = true_block.label,
		false_label = false_block.label,
	}
	get_last_block(ctx).terminator = jump

	add_block(ctx, true_block)

	gen_stmt(ctx, stmt.consequence)

	join_label: string
	// If there's a "else" block in this if statement, then we'll need to add a "join" label after it so that
	// when if the "consequence" block finished executing it knows to skip the "else" block.
	// Exception is when there's a return statement inside the consquence block.
	if stmt.alternative != nil && get_last_block(ctx).terminator == nil {
		join_label = make_label(ctx, "if_join")
		jump := make_jump(ctx, .Jmp)
		jump.data = JmpData{join_label}
		get_last_block(ctx).terminator = jump
	}

	add_block(ctx, false_block)

	if stmt.alternative != nil {
		gen_stmt(ctx, stmt.alternative)
		if join_label != "" {
			join_block := make_block(ctx, "")
			join_block.label = join_label
			add_block(ctx, join_block)
		}
	}
}

gen_empty_arr :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) -> Operand {
	empty_arr := new(ast.Array, ctx.allocator)
	empty_arr.node = ast.Node {
		tok           = stmt.declared_type.tok,
		resolved_type = stmt.resolved_type,
	}
	return gen_array_expr(ctx, empty_arr)
}

gen_struct_assign :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) {
	// if stmt.value == nil {
	//
	// }

	error(
		ctx,
		stmt.tok,
		"TODO: non-empty struct assignments not implemented yet",
	)
}

gen_array_assign :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) {
	operand :=
		stmt.value != nil ? gen_expr(ctx, stmt.value) : gen_empty_arr(ctx, stmt)
	push_symbol_entry(
		ctx,
		name = stmt.name.value,
		kind = .Local,
		typeinfo = stmt.resolved_type,
		op = operand,
	)
}

gen_assign_stmt :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) {
	if len(ctx.symbol_stack) == 1 {
		gen_global_assign(ctx, stmt)
		return
	}

	if stmt.resolved_type.kind == .Array {
		gen_array_assign(ctx, stmt)
		return
	}

	if stmt.resolved_type.kind == .Struct {
		gen_struct_assign(ctx, stmt)
		return
	}

	block := get_last_block(ctx)
	dest_name := make_temp(ctx)
	alloc_inst := make_instruction(
		ctx,
		.Alloc,
		alignment = typeinfo_to_size(stmt.resolved_type),
		dest = Operand{.Temporary, dest_name},
		dest_type = .Long,
		src1 = Operand{.Integer, typeinfo_to_size(stmt.resolved_type)},
	)
	append(&block.instructions, alloc_inst)

	src1 := Operand{.Integer, 0}
	if stmt.value != nil {
		src1 = gen_expr(ctx, stmt.value)
	}
	src2 := Operand{.Temporary, dest_name}

	store_inst := make_instruction(
		ctx,
		.Store,
		opcode_type = type_ast_to_ir(stmt.resolved_type.kind),
		src1 = src1,
		src2 = src2,
	)
	append(&block.instructions, store_inst)

	push_symbol_entry(
		ctx,
		name = stmt.name.value,
		kind = .Local,
		typeinfo = stmt.resolved_type,
		op = Operand{kind = .Temporary, data = dest_name},
	)
}

gen_global_assign :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) {
	if stmt.resolved_type.kind != .String &&
	   stmt.resolved_type.kind != .Bool &&
	   stmt.resolved_type.kind != .Int &&
	   stmt.resolved_type.kind != .Array {
		error(
			ctx,
			stmt.tok,
			"Unsupported constant - global constants can only be Array, String, Bool, or Int",
		)
		return
	}

	data := make_data(ctx)
	data.name = stmt.name.value
	data.linkage = .None
	if stmt.value == nil {
		field := DataZeroInit{typeinfo_to_size(stmt.resolved_type)}
		data.content = make_data_content(field)
	} else {
		#partial switch stmt.resolved_type.kind {
		case .Array:
			error(
				ctx,
				stmt.tok,
				"TODO: Global arrays with initialized values not support yet",
			)
			return
		case .String:
			str := stmt.value.(^ast.StringLiteral)
			data.content = make_null_terminated_str_data_content(str.value)
		case:
			content := DataContent{}
			field := DataInit {
				type = type_ast_to_ir(stmt.resolved_type.kind),
			}
			op := gen_expr(ctx, stmt.value)
			// TODO: I'm converting the operand data union into a DataItem union. these are both union{string, int}
			//       investigate more about wrapping this union to be the correct type (is this type-casting? what happens when either union changes?)
			append(&field.items, DataItem(op.data))
			append(&content.fields, field)
			data.content = content
		}
	}

	push_symbol_entry(
		ctx,
		name = stmt.name.value,
		kind = .Global,
		typeinfo = stmt.resolved_type,
		op = Operand{kind = .GlobalSymbol, data = stmt.name.value},
	)
	push_data(ctx, data)
	return
}

gen_expr :: proc(ctx: ^Context, expr: ast.Expr) -> Operand {
	switch e in expr {
	case ^ast.IntLiteral:
		return gen_integer(e.value)
	case ^ast.Boolean:
		return gen_integer(e.value ? 1 : 0)
	case ^ast.StringLiteral:
		return gen_string_literal(ctx, e)
	case ^ast.Identifier:
		return gen_identifier(ctx, e)
	case ^ast.ReassignExpr:
		return gen_reassign(ctx, e)
	case ^ast.PrefixExpr:
		return gen_prefix_expr(ctx, e)
	case ^ast.InfixExpr:
		return gen_infix_expr(ctx, e)
	case ^ast.CallExpr:
		return gen_call_expr(ctx, e)
	case ^ast.Array:
		return gen_array_expr(ctx, e)
	case ^ast.IndexExpr:
		return gen_index_expr(ctx, e)
	case ^ast.PointerExpr:
		addr := get_lvalue_addr(ctx, e.operand)
		return addr
	case ^ast.DerefExpr:
	}
	typeinfo := ast.get_resolved_type_from_expr(expr)
	tok := ast.get_token_from_expr(expr)
	error(ctx, tok, "Unhandled expression type %s", typeinfo.kind)
	return invalid_op()
}

gen_index_expr :: proc(ctx: ^Context, expr: ^ast.IndexExpr) -> Operand {
	addr_operand := gen_index_addr(ctx, expr)
	if addr_operand.kind == .Invalid {
		return addr_operand
	}

	type := type_ast_to_ir(expr.resolved_type.kind)

	dest := Operand{.Temporary, make_temp(ctx)}
	inst := make_instruction(
		ctx,
		.Load,
		dest = dest,
		dest_type = type,
		opcode_type = type,
		src1 = addr_operand,
	)

	block := get_last_block(ctx)
	append(&block.instructions, inst)
	return dest
}

gen_index_addr :: proc(ctx: ^Context, expr: ^ast.IndexExpr) -> Operand {
	ident, ok := expr.left.(^ast.Identifier)
	if !ok {
		tok := ast.get_token_from_expr(expr.left)
		error(
			ctx,
			tok,
			"Expected identifier when using index expression, got %s",
			tok.type,
		)
		return invalid_op()
	}
	symbol, okk := lookup_symbol(ctx, ident.value)
	if !okk {
		error(ctx, ident.tok, "Identifier not found: %s", ident.value)
		return invalid_op()
	}
	if symbol.typeinfo.kind != .Array {
		error(ctx, ident.tok, "Expected array, got %s", symbol.typeinfo.kind)
		return invalid_op()
	}

	index_operand := gen_expr(ctx, expr.index)

	arr_typeinfo := symbol.typeinfo.data.(ast.ArrayTypeInfo)
	element_size := typeinfo_to_size(arr_typeinfo.elements_type)
	block := get_last_block(ctx)

	// multiply index by the element size to get the array offset
	mul_dest := Operand{.Temporary, make_temp(ctx)}
	mul_inst := make_instruction(
		ctx,
		opcode = .Mul,
		dest = mul_dest,
		dest_type = .Word,
		src1 = index_operand,
		src2 = Operand{.Integer, element_size},
	)
	append(&block.instructions, mul_inst)

	// convert from .Word to .Long so its able to be added to the array pointer
	conv_dest := Operand{.Temporary, make_temp(ctx)}
	conv_inst := make_instruction(
		ctx,
		opcode = .Convert,
		conversion_type = .EXT_SIGNED_WORD,
		dest = conv_dest,
		dest_type = .Long,
		src1 = mul_dest,
	)
	append(&block.instructions, conv_inst)

	// add to the array pointer
	add_dest := Operand{.Temporary, make_temp(ctx)}
	add_inst := make_instruction(
		ctx,
		opcode = .Add,
		dest = add_dest,
		dest_type = .Long,
		src1 = symbol.op,
		src2 = conv_dest,
	)
	append(&block.instructions, add_inst)

	return add_dest
}

gen_array_expr_elements :: proc(
	ctx: ^Context,
	arr: ^ast.Array,
	head: Operand,
) {
	typeinfo := arr.resolved_type.data.(ast.ArrayTypeInfo)
	alignment := typeinfo_to_size(typeinfo.elements_type)
	element_type := type_ast_to_ir(typeinfo.elements_type.kind)

	for el_expr, idx in arr.elements {
		offset := alignment * idx
		src1 := gen_expr(ctx, el_expr)

		// If index is 0, we want to store the head pointer
		// otherwise do some pointer arithmatic to get the correct offset and use that instead
		src2 := head
		if idx > 0 {
			add_dest := Operand{.Temporary, make_temp(ctx)}
			add_inst := make_instruction(
				ctx,
				opcode = .Add,
				dest = add_dest,
				dest_type = .Long,
				src1 = head,
				src2 = Operand{.Integer, offset},
			)
			add_instruction(ctx, add_inst)
			src2 = add_dest
		}

		store_inst := make_instruction(
			ctx,
			opcode = .Store,
			opcode_type = element_type,
			src1 = src1,
			src2 = src2,
		)
		add_instruction(ctx, store_inst)
	}
}

//
// TODO: handle zero-initialization
//
gen_array_expr :: proc(ctx: ^Context, arr: ^ast.Array) -> Operand {
	head_dest := Operand{.Temporary, make_temp(ctx)}

	typeinfo := arr.resolved_type.data.(ast.ArrayTypeInfo)
	alloc_inst := make_instruction(
		ctx,
		.Alloc,
		dest = head_dest,
		dest_type = type_ast_to_ir(arr.resolved_type.kind),
		src1 = Operand{.Integer, typeinfo_to_size(arr.resolved_type)},
		alignment = typeinfo_to_size(typeinfo.elements_type),
	)
	add_instruction(ctx, alloc_inst)

	gen_array_expr_elements(ctx, arr, head_dest)

	return head_dest
}

gen_integer :: proc(data: int) -> Operand {
	return Operand{kind = .Integer, data = data}
}

//
// TODO: need better support for libc builtins
//
gen_printf :: proc(ctx: ^Context, expr: ^ast.CallExpr) -> Operand {
	dest := Operand{.Temporary, make_temp(ctx)}
	inst := make_instruction(
		ctx,
		opcode = .Call,
		src1 = Operand{.GlobalSymbol, expr.func.value},
		dest = dest,
		dest_type = .Word,
	)

	call_args := gen_call_args(ctx, expr)
	if len(call_args) > 1 {
		inject_at(&call_args, 1, CallArgument{kind = .Variadic})
	}
	inst.call_args = call_args

	add_instruction(ctx, inst)
	return dest
}

gen_call_args :: proc(
	ctx: ^Context,
	expr: ^ast.CallExpr,
	func_typeinfo: ^ast.FunctionTypeInfo = nil,
) -> [dynamic]CallArgument {
	call_args := make([dynamic]CallArgument, ctx.allocator)
	for arg, idx in expr.args {
		op := gen_expr(ctx, arg)
		typeinfo := ast.get_resolved_type_from_expr(arg)

		call_arg_value: Operand

		// implicitly convert arrays to slices, only if the function signature is expecting a slice and we're trying to provide an array
		if func_typeinfo != nil &&
		   func_typeinfo.param_types[idx].kind == .Slice &&
		   typeinfo.kind == .Array {
			arr_tc := typeinfo.data.(ast.ArrayTypeInfo)
			call_arg := CallArgument {
				kind           = .Regular,
				type           = .Aggregate,
				aggregate_name = SLICE_TYPE_NAME,
				value          = gen_slice(
					ctx,
					op,
					Operand{.Integer, arr_tc.size},
				),
			}
			append(&call_args, call_arg)
		} else {
			call_arg := CallArgument {
				kind  = .Regular,
				type  = type_ast_to_ir(typeinfo.kind),
				value = op,
			}
			if typeinfo.kind == .Slice {
				call_arg.aggregate_name = SLICE_TYPE_NAME
			}
			append(&call_args, call_arg)
		}
	}
	return call_args
}

gen_call_expr :: proc(ctx: ^Context, expr: ^ast.CallExpr) -> Operand {
	if expr.func.value == "printf" {
		return gen_printf(ctx, expr)
	}

	func, found := lookup_symbol(ctx, expr.func.value)
	if !found {
		error(ctx, expr.tok, "Identifier not found %s", expr.func.value)
		return invalid_op()
	}

	if func.kind != .Func {
		error(
			ctx,
			expr.tok,
			"Call expression from something thats not a function, got %s",
			func.kind,
		)
		return invalid_op()
	}
	func_typeinfo := func.typeinfo.data.(ast.FunctionTypeInfo)

	dest: Maybe(Operand) = nil
	dest_type: Maybe(TypeKind) = nil

	if func_typeinfo.return_type.kind != .Void {
		dest = Operand{.Temporary, make_temp(ctx)}
		dest_type = type_ast_to_ir(func.typeinfo.kind)
	}

	inst := make_instruction(
		ctx,
		opcode = .Call,
		src1 = func.op,
		dest = dest,
		dest_type = dest_type,
	)
	inst.call_args = gen_call_args(ctx, expr, &func_typeinfo)
	add_instruction(ctx, inst)

	d, ok := dest.?
	if !ok {
		return Operand{}
	}
	return d
}

gen_infix_expr :: proc(ctx: ^Context, expr: ^ast.InfixExpr) -> Operand {
	lhs := gen_expr(ctx, expr.left)
	rhs := gen_expr(ctx, expr.right)
	if lhs.kind == .Invalid || rhs.kind == .Invalid {
		logger.error("One of the expressions was invalid")
		return invalid_op()
	}

	lhs_typeinfo := ast.get_resolved_type_from_expr(expr.left)
	rhs_typeinfo := ast.get_resolved_type_from_expr(expr.right)
	if lhs_typeinfo.kind != rhs_typeinfo.kind {
		logger.error(
			"Infix expressions can only be done on expressions of the same type",
		)
		return invalid_op()
	}

	dest := Operand{.Temporary, make_temp(ctx)}
	type := type_ast_to_ir(lhs_typeinfo.kind)

	dest_type: Maybe(TypeKind) = type
	opcode_type: Maybe(TypeKind) = type

	opcode: OpCode
	comparison_type: ComparisonType = .None
	#partial switch expr.tok.type {
	case .AND, .OR:
		return gen_logical_operators(ctx, expr)
	case .PLUS:
		opcode = .Add
		opcode_type = nil
	case .MINUS:
		opcode = .Sub
		opcode_type = nil
	case .STAR:
		opcode = .Mul
		opcode_type = nil
	case .SLASH:
		opcode = .Div
		opcode_type = nil
	case .EQ:
		opcode = .Compare
		comparison_type = .Equal
	case .NOT_EQ:
		opcode = .Compare
		comparison_type = .NotEqual
	case .GT:
		opcode = .Compare
		comparison_type = .SignedGreater
	case .GTE:
		opcode = .Compare
		comparison_type = .SignedGreaterEqual
	case .LT:
		opcode = .Compare
		comparison_type = .SignedLess
	case .LTE:
		opcode = .Compare
		comparison_type = .SignedLessEqual
	case:
		logger.error("Unsupported operator: %s", expr.tok.type)
		return invalid_op()
	}

	inst := make_instruction(
		ctx,
		dest = dest,
		dest_type = dest_type,
		opcode = opcode,
		opcode_type = opcode_type,
		comparison_type = comparison_type,
		src1 = lhs,
		src2 = rhs,
	)

	block := get_last_block(ctx)
	append(&block.instructions, inst)

	return dest
}

gen_logical_operators :: proc(ctx: ^Context, expr: ^ast.InfixExpr) -> Operand {
	logic_right := make_block(ctx, "logic_right")
	true_merge := make_block(ctx, "true_merge")
	false_merge := make_block(ctx, "false_merge")
	assign_merge := make_block(ctx, "assign_merge")

	left_operand := gen_expr(ctx, expr.left)
	is_and := expr.tok.type == .AND
	first_jnz := make_jump(ctx, .Jnz)
	first_jnz.data = JnzData {
		condition   = left_operand,
		true_label  = is_and ? logic_right.label : true_merge.label,
		false_label = is_and ? false_merge.label : logic_right.label,
	}
	get_last_block(ctx).terminator = first_jnz

	add_block(ctx, logic_right)
	right_operand := gen_expr(ctx, expr.right)

	second_jnz := make_jump(ctx, .Jnz)
	second_jnz.data = JnzData {
		condition   = right_operand,
		true_label  = true_merge.label,
		false_label = false_merge.label,
	}
	get_last_block(ctx).terminator = second_jnz

	add_block(ctx, true_merge)
	jmp := make_jump(ctx, .Jmp)
	jmp.data = JmpData{assign_merge.label}
	get_last_block(ctx).terminator = jmp

	add_block(ctx, false_merge)
	get_last_block(ctx).terminator = jmp

	add_block(ctx, assign_merge)

	result_operand := Operand{.Temporary, make_temp(ctx)}
	inst := make_phi(
		ctx,
		dest = result_operand,
		dest_type = type_ast_to_ir(expr.resolved_type.kind),
		src1_label = true_merge.label,
		src1_operand = Operand{.Integer, 1},
		src2_label = false_merge.label,
		src2_operand = Operand{.Integer, 0},
	)
	b := get_last_block(ctx)
	append(&b.instructions, inst)

	return result_operand
}

gen_prefix_expr :: proc(ctx: ^Context, expr: ^ast.PrefixExpr) -> Operand {
	block := get_last_block(ctx)

	rvalue := gen_expr(ctx, expr.right)
	rvalue_typeinfo := ast.get_resolved_type_from_expr(expr.right)
	copy_dest := Operand{.Temporary, make_temp(ctx)}
	copy_inst := make_instruction(
		ctx,
		.Copy,
		dest = copy_dest,
		dest_type = type_ast_to_ir(rvalue_typeinfo.kind),
		src1 = rvalue,
	)
	append(&block.instructions, copy_inst)

	dest := Operand{}
	#partial switch expr.tok.type {
	case .MINUS:
		dest = Operand{.Temporary, make_temp(ctx)}
		neg_inst := make_instruction(
			ctx,
			.Neg,
			dest = dest,
			dest_type = type_ast_to_ir(rvalue_typeinfo.kind),
			src1 = copy_dest,
		)
		append(&block.instructions, neg_inst)
	case .BANG:
		dest = Operand{.Temporary, make_temp(ctx)}
		not_inst := make_instruction(
			ctx,
			.Compare,
			comparison_type = .Equal,
			opcode_type = type_ast_to_ir(rvalue_typeinfo.kind),
			dest = dest,
			dest_type = type_ast_to_ir(rvalue_typeinfo.kind),
			src1 = copy_dest,
			src2 = Operand{.Integer, 0},
		)
		append(&block.instructions, not_inst)
	}
	return dest
}

gen_reassign_array :: proc(ctx: ^Context, expr: ^ast.ReassignExpr) -> Operand {
	lvalue := get_lvalue_addr(ctx, expr.target)
	arr := expr.value.(^ast.Array)
	gen_array_expr_elements(ctx, arr, lvalue)
	return lvalue
}

gen_reassign :: proc(ctx: ^Context, expr: ^ast.ReassignExpr) -> Operand {
	lvalue_typeinfo := ast.get_resolved_type_from_expr(expr.target)
	if lvalue_typeinfo.kind == .Array {
		return gen_reassign_array(ctx, expr)
	}

	lvalue := get_lvalue_addr(ctx, expr.target)
	rvalue := gen_expr(ctx, expr.value)

	rvalue_typeinfo := ast.get_resolved_type_from_expr(expr.value)
	inst := make_instruction(
		ctx,
		.Store,
		opcode_type = type_ast_to_ir(rvalue_typeinfo.kind),
		src1 = rvalue,
		src2 = lvalue,
	)

	block := get_last_block(ctx)
	append(&block.instructions, inst)

	return lvalue
}

get_lvalue_addr :: proc(ctx: ^Context, target: ast.Expr) -> Operand {
	#partial switch e in target {
	case ^ast.Identifier:
		ident, found := lookup_symbol(ctx, e.value)
		if !found {
			error(ctx, e.tok, "lvalue undefined identifier: %s", e.value)
			return invalid_op()
		}
		return ident.op
	case ^ast.IndexExpr:
		return gen_index_addr(ctx, e)
	case ^ast.DerefExpr:
		return gen_expr(ctx, e.operand)
	}
	tok := ast.get_token_from_expr(target)
	error(ctx, tok, "%s is not a valid lvalue", tok.type)
	return invalid_op()
}

gen_identifier :: proc(ctx: ^Context, expr: ^ast.Identifier) -> Operand {
	ident, found := lookup_symbol(ctx, expr.value)
	ir_kind := type_ast_to_ir(ident.typeinfo.kind)
	if !found {
		error(ctx, expr.tok, "undefined identifier: %s", expr.value)
		return Operand{.Invalid, -1}
	}

	block := get_last_block(ctx)

	switch ident.kind {
	case .FuncParam, .Func:
		return ident.op
	case .Local, .Global:
		if ident.typeinfo.kind == .Array || ident.typeinfo.kind == .Slice {
			return ident.op
		}

		dest := Operand{.Temporary, make_temp(ctx)}
		inst := make_instruction(
			ctx,
			.Load,
			dest_type = ir_kind,
			opcode_type = ir_kind,
			src1 = ident.op,
			dest = dest,
		)
		append(&block.instructions, inst)
		return dest
	}
	panic("unhandled ident kind in gen_identifer")
}

gen_string_literal :: proc(ctx: ^Context, e: ^ast.StringLiteral) -> Operand {
	def := make_data(ctx)
	def.name = make_string_label(ctx)
	def.linkage = .None
	def.content = make_null_terminated_str_data_content(e.value)
	push_data(ctx, def)
	return Operand{kind = .GlobalSymbol, data = def.name}
}

gen_return_stmt :: proc(ctx: ^Context, ret: ^ast.ReturnStatement) {
	if len(ctx.symbol_stack) == 1 {
		error(ctx, ret.tok, "Return statement found outside of a function")
		return
	}

	jump := make_jump(ctx, .Ret)
	if ret.value != nil {
		value := gen_expr(ctx, ret.value)
		jump.data = RetData{value}
	} else {
		jump.data = RetData{}
	}

	get_last_block(ctx).terminator = jump
}

gen_block_stmt :: proc(ctx: ^Context, block: ^ast.BlockStatement) {
	for stmt in block.stmts {
		gen_stmt(ctx, stmt)
	}
}

gen_function_stmt :: proc(ctx: ^Context, stmt: ^ast.FunctionStatement) {
	ctx.next_temp_id = 0
	ctx.next_label_id = 0

	def := new(FunctionDef, ctx.allocator)
	typeinfo := stmt.resolved_type.data.(ast.FunctionTypeInfo)
	is_main := stmt.name.value == "main"

	if is_main {
		def.name = USER_MAIN_NAME
	} else {
		def.name = stmt.name.value
	}

	def.linkage = .None
	def.return_type = type_ast_to_ir(typeinfo.return_type.kind)

	push_symbol_entry(
		ctx,
		name = def.name,
		kind = .Func,
		typeinfo = stmt.resolved_type,
		op = Operand{.GlobalSymbol, def.name},
	)

	push_symbol_scope(ctx)
	defer pop_symbol_scope(ctx)

	for arg in stmt.args {
		param := FunctionParam {
			type = type_ast_to_ir(arg.resolved_type.kind),
			op   = Operand{.Temporary, make_temp(ctx)},
		}
		if arg.resolved_type.kind == .Slice {
			param.aggregate_name = SLICE_TYPE_NAME
		}
		push_symbol_entry(
			ctx,
			name = arg.ident.value,
			kind = .FuncParam,
			typeinfo = arg.resolved_type,
			op = param.op,
		)
		append(&def.params, param)
	}

	block := make_block(ctx, "start")
	append(&def.blocks, block)
	append(&ctx.module.functions, def)

	gen_stmt(ctx, stmt.body)

	if get_last_block(ctx).terminator == nil {
		if is_main {
			jump := make_jump(ctx, .Ret)
			operand := Operand{.Integer, 0}
			jump.data = RetData{operand}
			get_last_block(ctx).terminator = jump
		} else if def.return_type == .Void {
			jump := make_jump(ctx, .Ret)
			jump.data = RetData{}
			block := get_last_block(ctx)
			get_last_block(ctx).terminator = jump
		} else {
			error(
				ctx,
				stmt.tok,
				"Non-void function does not return a value. Expected return type %s",
				def.return_type,
			)
		}
	}
}

make_temp :: proc(ctx: ^Context) -> string {
	ctx.next_temp_id += 1

	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, ".%d", ctx.next_temp_id)
	return strings.to_string(sb)
}

make_label :: proc(ctx: ^Context, name: string) -> string {
	ctx.next_label_id += 1

	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "%s.%d", name, ctx.next_label_id)
	return strings.to_string(sb)
}

add_block :: proc(ctx: ^Context, block: ^Block) {
	func := ctx.module.functions[len(ctx.module.functions) - 1]
	append(&func.blocks, block)
}

get_last_block :: proc(ctx: ^Context) -> ^Block {
	func := ctx.module.functions[len(ctx.module.functions) - 1]
	block := func.blocks[len(func.blocks) - 1]
	return block
}

invalid_op :: proc() -> Operand {
	return Operand{.Invalid, -1}
}

make_jump :: proc(ctx: ^Context, kind: JumpType) -> ^Jump {
	jump := new(Jump, ctx.allocator)
	jump.kind = kind
	return jump
}

make_string_label :: proc(ctx: ^Context) -> string {
	ctx.str_count += 1
	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "str_%d", ctx.str_count)
	return strings.to_string(sb)
}

make_null_terminated_str_data_content :: proc(str: string) -> DataContent {
	content := DataContent{}
	str_field := DataInit {
		type = .Byte,
	}
	append(&str_field.items, str)
	append(&content.fields, str_field)
	null_term_field := DataInit {
		type = .Byte,
	}
	append(&null_term_field.items, 0)
	append(&content.fields, null_term_field)
	return content
}

make_phi :: proc(
	ctx: ^Context,
	dest: Maybe(Operand) = nil,
	dest_type: Maybe(TypeKind) = nil,
	src1_label: string,
	src1_operand: Operand,
	src2_label: string,
	src2_operand: Operand,
) -> ^Instruction {
	inst := new(Instruction, ctx.allocator)
	inst.opcode = .Phi
	inst.dest = dest
	inst.dest_type = dest_type
	sources := new(PhiSources, ctx.allocator)
	sources.src1_label = src1_label
	sources.src1_operand = src1_operand
	sources.src2_label = src2_label
	sources.src2_operand = src2_operand
	inst.phi_sources = sources
	return inst
}

make_instruction :: proc(
	ctx: ^Context,
	opcode: OpCode,
	src1: Operand,
	src2: Maybe(Operand) = nil,
	dest: Maybe(Operand) = nil,
	dest_type: Maybe(TypeKind) = nil,
	opcode_type: Maybe(TypeKind) = nil,
	comparison_type: ComparisonType = .None,
	conversion_type: ConversionType = .None,
	alignment: int = 0,
) -> ^Instruction {
	inst := new(Instruction, ctx.allocator)
	inst.opcode = opcode
	inst.src1 = src1
	inst.src2 = src2
	inst.dest = dest
	inst.opcode_type = opcode_type
	inst.dest_type = dest_type
	inst.comparison_type = comparison_type
	inst.alignment = alignment
	inst.conversion_type = conversion_type
	return inst
}

add_instruction :: proc(ctx: ^Context, inst: ^Instruction) {
	block := get_last_block(ctx)
	append(&block.instructions, inst)
}

make_typedef :: proc(
	ctx: ^Context,
	name: string,
	alignment: int = 0,
	content: TypeDefContent = nil,
) -> ^TypeDef {
	td := new(TypeDef, ctx.allocator)
	td.name = name
	td.alignment = alignment
	td.content = content
	return td
}

add_typedef :: proc(ctx: ^Context, td: ^TypeDef) {
	append(&ctx.module.types, td)
}

make_data_content :: proc(fields: ..DataField) -> DataContent {
	content := DataContent{}
	append(&content.fields, ..fields)
	return content
}

make_data :: proc(ctx: ^Context) -> ^DataDef {
	def := new(DataDef, ctx.allocator)
	return def
}

push_data :: proc(ctx: ^Context, data: ^DataDef) {
	append(&ctx.module.data, data)
}

make_block :: proc(ctx: ^Context, label: string) -> ^Block {
	block := new(Block, ctx.allocator)
	if label != "" {
		block.label = make_label(ctx, label)
	}
	return block
}

lookup_symbol :: proc(ctx: ^Context, name: string) -> (^SymbolEntry, bool) {
	for i := len(ctx.symbol_stack) - 1; i >= 0; i -= 1 {
		scope := ctx.symbol_stack[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

push_symbol_scope :: proc(ctx: ^Context) {
	scope := make(SymbolTable, ctx.allocator)
	append(&ctx.symbol_stack, scope)
}

pop_symbol_scope :: proc(ctx: ^Context) {
	if len(ctx.symbol_stack) <= 0 {
		// TODO: better error handling when we don't have a token
		panic("attempting to pop scope from an empty symbol stack")
	}
	popped := pop(&ctx.symbol_stack)
	delete(popped)
}

push_symbol_entry :: proc(
	ctx: ^Context,
	name: string,
	kind: SymbolKind,
	typeinfo: ^ast.TypeInfo,
	op: Operand,
) {
	entry := new(SymbolEntry, ctx.allocator)
	entry.name = name
	entry.kind = kind
	entry.typeinfo = typeinfo
	entry.op = op
	ctx.symbol_stack[len(ctx.symbol_stack) - 1][entry.name] = entry
}

push_loop_end_label :: proc(ctx: ^Context, label: string) {
	append(&ctx.loop_end_labels, label)
}

pop_loop_end_label :: proc(ctx: ^Context) {
	if len(ctx.loop_end_labels) <= 0 {
		panic("attempting to pop scope from an empty loop_end stack")
	}
	pop(&ctx.loop_end_labels)
}

type_ast_to_ir :: proc(type: ast.TypeKind) -> TypeKind {
	switch type {
	case .String:
		return .Long
	case .Int:
		return .Word
	case .Bool:
		return .Word
	case .Function:
		return .Long
	case .Void:
		return .Void
	case .Array:
		return .Long
	case .Slice:
		return .Aggregate
	case .Pointer:
		return .Long
	case .Struct:
		return .Aggregate
	case .Invalid, .Any:
		panic("ERROR converting ast to ir - Invalid/Any type")
	}
	panic("Unhandled ast type in ir.type_ast_to_ir")
}

typeinfo_to_size :: proc(typeinfo: ^ast.TypeInfo) -> int {
	switch typeinfo.kind {
	case .Struct:
		// TODO
		return 0
	case .Array:
		arr_typeinfo := typeinfo.data.(ast.ArrayTypeInfo)
		return typeinfo_to_size(arr_typeinfo.elements_type) * arr_typeinfo.size
	case .Slice:
		// is it ok to hard-code this? :Slice = { l, w } aka alloc8 16
		return 16
	case .Int, .Bool:
		return 4
	case .String, .Pointer:
		return 8
	case .Function:
		// TODO: function size should refer to a pointer maybe?
		return 0
	case .Void, .Invalid, .Any:
		return 0
	}
	return 0
}

error :: proc(ctx: ^Context, tok: token.Token, ft: string, args: ..any) {
	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, ft, ..args)
	msg := strings.to_string(sb)

	err := logger.CompilerError {
		msg  = msg,
		line = tok.line,
		col  = tok.col,
	}
	append(&ctx.errors, err)
}
