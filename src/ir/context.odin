package ir


import "../ast"
import "../logger"
import "core:fmt"
import "core:mem"
import "core:strings"

new_context :: proc(allocator: mem.Allocator) -> ^Context {
	ctx := new(Context, allocator)
	ctx.allocator = allocator
	ctx.module = new(Module, allocator)
	return ctx
}

from_ast :: proc(c: ^Context, module: ^ast.Module) {
	for stmt in module.stmts {
		gen_stmt(c, stmt)
	}
}

gen_stmt :: proc(ctx: ^Context, stmt: ast.Statement) {
	#partial switch v in stmt {
	case ^ast.Module:
		panic("Unexpected module")
	case ^ast.FunctionStatement:
		gen_function_stmt(ctx, v)
	case ^ast.BlockStatement:
		gen_block_stmt(ctx, v)
	case ^ast.ReturnStatement:
		gen_return_stmt(ctx, v)
	}
}

gen_expr :: proc(ctx: ^Context, expr: ast.Expr) -> Operand {
	#partial switch e in expr {
	case ^ast.IntLiteral:
		return Operand{kind = .LiteralConst, data = OperandConstantData{value = e.value}}
	case:
		panic("Unhandled expression")
	}
}

gen_return_stmt :: proc(ctx: ^Context, ret: ^ast.ReturnStatement) {
	jump := new_jump(ctx, .Ret)
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
	// TODO def.params
	block := new_block(ctx)
	block.label = new_label(ctx, "start")
	append(&def.blocks, block)
	append(&ctx.module.functions, def)
	gen_stmt(ctx, stmt.body)
}

get_last_block :: proc(ctx: ^Context) -> ^Block {
	func := ctx.module.functions[len(ctx.module.functions) - 1]
	block := func.blocks[len(func.blocks) - 1]
	return block
}

new_jump :: proc(ctx: ^Context, kind: JumpType) -> ^Jump {
	jump := new(Jump, ctx.allocator)
	jump.kind = kind
	return jump
}

new_label :: proc(ctx: ^Context, name: string) -> string {
	ctx.next_label_id += 1
	sb: strings.Builder
	strings.builder_init(&sb, ctx.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "@%s.%d", name, ctx.next_label_id)
	return strings.to_string(sb)
}

new_block :: proc(ctx: ^Context) -> ^Block {
	block := new(Block, ctx.allocator)
	return block
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
