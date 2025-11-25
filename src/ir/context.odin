package ir

import "../ast"
import "../logger"
import "../token"
import "core:fmt"
import "core:mem"
import "core:strings"

Context :: struct {
	allocator:    mem.Allocator,
	module:       ^Module,
	errors:       [dynamic]logger.CompilerError,
	str_count:    int,
	symbols:      [dynamic]SymbolTable,
	next_temp_id: int,
	temp_ids:     map[string]int,
}

SymbolTable :: map[string]^SymbolEntry
SymbolEntry :: struct {
	name:     string,
	kind:     SymbolKind,
	ir_kind:  TypeKind,
	typeinfo: ^ast.TypeInfo,
	op:       Operand,
}
SymbolKind :: enum {
	Local,
	Global,
	Func,
	FuncParam,
}

new_context :: proc(allocator: mem.Allocator) -> ^Context {
	ctx := new(Context, allocator)
	ctx.allocator = allocator
	ctx.module = new(Module, allocator)
	ctx.errors = make([dynamic]logger.CompilerError, allocator)
	push_symbol_scope(ctx)
	return ctx
}

from_ast :: proc(c: ^Context, module: ^ast.Module) {
	for stmt in module.stmts {
		gen_stmt(c, stmt)
	}
}

gen_stmt :: proc(ctx: ^Context, stmt: ast.Statement) {
	switch v in stmt {
	case ^ast.Module:
		error(ctx, v.tok, "Unexpected module")
		return
	case ^ast.FunctionStatement:
		gen_function_stmt(ctx, v)
	case ^ast.BlockStatement:
		gen_block_stmt(ctx, v)
	case ^ast.ReturnStatement:
		gen_return_stmt(ctx, v)
	case ^ast.ExprStatement:
		gen_expr(ctx, v.value)
	case ^ast.AssignStatement:
		gen_assign_stmt(ctx, v)
	case ^ast.FunctionArg:
		error(ctx, v.tok, "Unreachable - function_arg")
	case ^ast.IfStatement:
		error(ctx, v.tok, "TODO: implement IfStatement")
	case ^ast.LoopStatement:
		error(ctx, v.tok, "TODO: implement LoopStatement")
	case ^ast.BreakStatement:
		error(ctx, v.tok, "TODO: implement BreakStatement")
	}
}

gen_assign_stmt :: proc(ctx: ^Context, stmt: ^ast.AssignStatement) {
	if len(ctx.symbols) == 1 {
		gen_global_assign(ctx, stmt)
		return
	}

	block := get_last_block(ctx)

	dest_name := make_temp_name(ctx)
	alloc_inst := make_instruction(
		ctx,
		.Alloc,
		dest = Operand{.Temporary, dest_name},
		dest_type = .Long,
		src1 = Operand{.Integer, typeinfo_to_size(stmt.resolved_type)},
	)
	if stmt.resolved_type.kind == .Array {
		arr_typeinfo := stmt.resolved_type.data.(ast.ArrayTypeInfo)
		alloc_inst.alignment = type_to_size(arr_typeinfo.elements_type.kind)
	} else {
		alloc_inst.alignment = type_to_size(stmt.resolved_type.kind)
	}
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
		ir_kind = type_ast_to_ir(stmt.resolved_type.kind),
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
			error(ctx, stmt.tok, "TODO: Global arrays with initialized values not support yet")
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
		ir_kind = type_ast_to_ir(stmt.resolved_type.kind),
		typeinfo = stmt.resolved_type,
		op = Operand{kind = .GlobalSymbol, data = stmt.name.value},
	)
	push_data(ctx, data)
	return
}

gen_expr :: proc(ctx: ^Context, expr: ast.Expr) -> Operand {
	switch e in expr {
	case ^ast.IntLiteral:
		return Operand{kind = .Integer, data = e.value}
	case ^ast.Boolean:
		return Operand{kind = .Integer, data = e.value ? 1 : 0}
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
		error(ctx, e.tok, "TODO: implement Array")
	case ^ast.IndexExpr:
		error(ctx, e.tok, "TODO: implement IndexExpr")
	}
	return invalid_op()
}

//
// TODO: need better support for libc builtins
//
gen_printf :: proc(ctx: ^Context, expr: ^ast.CallExpr) -> Operand {
	dest := Operand{.Temporary, make_temp_name(ctx)}
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

	block := get_last_block(ctx)
	append(&block.instructions, inst)

	return dest
}

gen_call_args :: proc(ctx: ^Context, expr: ^ast.CallExpr) -> [dynamic]CallArgument {
	call_args := make([dynamic]CallArgument, ctx.allocator)
	for arg, idx in expr.args {
		op := gen_expr(ctx, arg)
		typeinfo := ast.get_resolved_type_from_expr(arg)
		call_arg := CallArgument {
			kind  = .Regular,
			type  = type_ast_to_ir(typeinfo.kind),
			value = op,
		}
		append(&call_args, call_arg)
	}
	return call_args
}

gen_call_expr :: proc(ctx: ^Context, expr: ^ast.CallExpr) -> Operand {
	if expr.func.value == "printf" {
		return gen_printf(ctx, expr)
	}

	ident, found := lookup_symbol(ctx, expr.func.value)
	if !found {
		error(ctx, expr.tok, "Identifier not found %s", expr.func.value)
		return invalid_op()
	}

	dest := Operand{.Temporary, make_temp_name(ctx)}
	inst := make_instruction(
		ctx,
		opcode = .Call,
		src1 = ident.op,
		dest = dest,
		dest_type = type_ast_to_ir(expr.resolved_type.kind),
	)
	inst.call_args = gen_call_args(ctx, expr)

	block := get_last_block(ctx)
	append(&block.instructions, inst)

	return dest
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
		logger.error("Infix expressions can only be done on expressions of the same type")
		return invalid_op()
	}

	dest := Operand{.Temporary, make_temp_name(ctx)}
	type := type_ast_to_ir(lhs_typeinfo.kind)

	dest_type: Maybe(TypeKind) = type
	opcode_type: Maybe(TypeKind) = type

	opcode: OpCode
	comparison_type: ComparisonType = .None
	#partial switch expr.tok.type {
	case .AND, .OR:
		logger.error("TODO: implement AND and OR infix expressions")
		return invalid_op()
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

gen_prefix_expr :: proc(ctx: ^Context, expr: ^ast.PrefixExpr) -> Operand {
	block := get_last_block(ctx)

	rvalue := gen_expr(ctx, expr.right)
	rvalue_typeinfo := ast.get_resolved_type_from_expr(expr.right)
	copy_dest := Operand{.Temporary, make_temp_name(ctx)}
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
		dest = Operand{.Temporary, make_temp_name(ctx)}
		neg_inst := make_instruction(
			ctx,
			.Neg,
			dest = dest,
			dest_type = type_ast_to_ir(rvalue_typeinfo.kind),
			src1 = copy_dest,
		)
		append(&block.instructions, neg_inst)
	case .BANG:
		dest = Operand{.Temporary, make_temp_name(ctx)}
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

gen_reassign :: proc(ctx: ^Context, expr: ^ast.ReassignExpr) -> Operand {
	lvalue := get_lvalue_addr(ctx, expr.target)
	rvalue := gen_expr(ctx, expr.value)
	rvalue_typeinfo := ast.get_resolved_type_from_expr(expr.value)
	block := get_last_block(ctx)
	inst := make_instruction(
		ctx,
		.Store,
		opcode_type = type_ast_to_ir(rvalue_typeinfo.kind),
		src1 = rvalue,
		src2 = lvalue,
	)
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
	// TODO
	}
	tok := ast.get_token_from_expr(target)
	error(ctx, tok, "%s is not a valid lvalue", tok.type)
	return invalid_op()
}

gen_identifier :: proc(ctx: ^Context, expr: ^ast.Identifier) -> Operand {
	ident, found := lookup_symbol(ctx, expr.value)
	if !found {
		error(ctx, expr.tok, "undefined identifier: %s", expr.value)
		return Operand{.Invalid, -1}
	}

	block := get_last_block(ctx)

	switch ident.kind {
	case .FuncParam, .Global, .Func:
		return ident.op
	case .Local:
		dest := Operand{.Temporary, make_temp_name(ctx)}
		inst := make_instruction(
			ctx,
			.Load,
			dest_type = ident.ir_kind,
			opcode_type = ident.ir_kind,
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
	jump := make_jump(ctx, .Ret)
	value := gen_expr(ctx, ret.value)
	jump.data = RetData{value}
	block := get_last_block(ctx)
	block.terminator = jump
}

gen_block_stmt :: proc(ctx: ^Context, block: ^ast.BlockStatement) {
	for stmt in block.stmts {
		gen_stmt(ctx, stmt)
	}
}

gen_function_stmt :: proc(ctx: ^Context, stmt: ^ast.FunctionStatement) {
	def := new(FunctionDef, ctx.allocator)
	typeinfo := stmt.resolved_type.data.(ast.FunctionTypeInfo)

	if stmt.name.value == "main" {
		def.linkage = .Export
	} else {
		def.linkage = .None
	}

	def.return_type = type_ast_to_ir(typeinfo.return_type.kind)
	def.name = stmt.name.value

	push_symbol_entry(
		ctx,
		name = def.name,
		kind = .Func,
		ir_kind = type_ast_to_ir(stmt.resolved_type.kind),
		typeinfo = stmt.resolved_type,
		op = Operand{.GlobalSymbol, def.name},
	)

	push_symbol_scope(ctx)
	defer pop_symbol_scope(ctx)
	clear(&ctx.temp_ids)

	for arg in stmt.args {
		param := FunctionParam {
			type = type_ast_to_ir(arg.resolved_type.kind),
			op   = Operand{.Temporary, make_temp_name(ctx)},
		}
		push_symbol_entry(
			ctx,
			name = arg.ident.value,
			kind = .FuncParam,
			ir_kind = param.type,
			typeinfo = arg.resolved_type,
			op = param.op,
		)
		append(&def.params, param)
	}

	block := make_block(ctx)
	block.label = "start"
	append(&def.blocks, block)
	append(&ctx.module.functions, def)
	gen_stmt(ctx, stmt.body)
}

make_temp_name :: proc(ctx: ^Context) -> string {
	ctx.next_temp_id += 1

	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, ".%d", ctx.next_temp_id)
	return strings.to_string(sb)
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

make_instruction :: proc(
	ctx: ^Context,
	opcode: OpCode,
	src1: Operand,
	src2: Maybe(Operand) = nil,
	dest: Maybe(Operand) = nil,
	dest_type: Maybe(TypeKind) = nil,
	opcode_type: Maybe(TypeKind) = nil,
	comparison_type: ComparisonType = .None,
) -> ^Instruction {
	inst := new(Instruction, ctx.allocator)
	inst.opcode = opcode
	inst.src1 = src1
	inst.src2 = src2
	inst.dest = dest
	inst.opcode_type = opcode_type
	inst.dest_type = dest_type
	inst.comparison_type = comparison_type
	return inst
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

make_block :: proc(ctx: ^Context) -> ^Block {
	block := new(Block, ctx.allocator)
	return block
}

lookup_symbol :: proc(ctx: ^Context, name: string) -> (^SymbolEntry, bool) {
	for i := len(ctx.symbols) - 1; i >= 0; i -= 1 {
		scope := ctx.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

push_symbol_scope :: proc(ctx: ^Context) {
	scope := make(SymbolTable, ctx.allocator)
	append(&ctx.symbols, scope)
}

pop_symbol_scope :: proc(ctx: ^Context) {
	if len(ctx.symbols) <= 0 {
		// TODO: better error handling when we don't have a token
		panic("attempting to pop scope from an empty symbol stack")
	}
	popped := pop(&ctx.symbols)
	delete(popped)
}

push_symbol_entry :: proc(
	ctx: ^Context,
	name: string,
	kind: SymbolKind,
	ir_kind: TypeKind,
	typeinfo: ^ast.TypeInfo,
	op: Operand,
) {
	entry := new(SymbolEntry, ctx.allocator)
	entry.name = name
	entry.kind = kind
	entry.ir_kind = ir_kind
	entry.typeinfo = typeinfo
	entry.op = op
	ctx.symbols[len(ctx.symbols) - 1][entry.name] = entry
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
	case .Invalid, .Any:
		panic("ERROR converting ast to ir - Invalid/Any type")
	}
	panic("Unhandled ast type in ir.type_ast_to_ir")
}

typeinfo_to_size :: proc(typeinfo: ^ast.TypeInfo) -> int {
	if typeinfo.kind == .Array {
		arr_typeinfo := typeinfo.data.(ast.ArrayTypeInfo)
		return type_to_size(typeinfo.kind) * arr_typeinfo.size
	} else {
		return type_to_size(typeinfo.kind)
	}
}

type_to_size :: proc(type: ast.TypeKind) -> int {
	switch type {
	case .Int:
		return 4
	case .Bool:
		return 4
	case .Array:
		return 8
	case .String:
		return 8
	case .Function:
		// TODO: Kind of assuming that maybe in the future, function size will refer to a function pointer
		return 8
	case .Void:
		return 0
	case .Any:
		// TODO: Any type - assuming that if we get an `any` at this point, maybe its a pointer?
		return 8
	case .Invalid:
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
