package qbe

import "core:fmt"
import "core:mem"
import "core:strings"

import "../../ir"

QbeCodegen :: struct {
	allocator: mem.Allocator,
	sb:        strings.Builder,
}

new_qbecodegen :: proc(allocator: mem.Allocator) -> ^QbeCodegen {
	qbe := new(QbeCodegen, allocator)
	qbe.allocator = allocator
	strings.builder_init(&qbe.sb, allocator)
	return qbe
}

from_ir :: proc(qbe: ^QbeCodegen, module: ^ir.Module) {
	for func in module.functions {
		gen_func(qbe, func)
	}
}

gen_func :: proc(qbe: ^QbeCodegen, func: ^ir.FunctionDef) {
	if func.linkage == .Export {
		emit(qbe, "export ")
	}
	emit(qbe, "function ")
	if func.return_type != .Void {
		emit(qbe, "%s ", type_to_str(func.return_type))
	}
	emit(qbe, "$%s(", func.name)
	// TODO: params
	emit(qbe, ") {{\n")
	for block in func.blocks {
		gen_block(qbe, block)
	}
	emit(qbe, "}")
}

gen_block :: proc(qbe: ^QbeCodegen, block: ^ir.Block) {
	emit(qbe, "%s\n", block.label)
	// TODO: instructions
	gen_jump(qbe, block.terminator)
}

gen_jump :: proc(qbe: ^QbeCodegen, jump: ^ir.Jump) {
	switch jump.kind {
	case .Jnz:
		panic("TODO: Jnz not implemented")
	case .Jmp:
		panic("TODO: Jmp not implemented")
	case .Ret:
		data := jump.data.(ir.RetData)
		op := gen_operand(qbe, data.val)
		emit(qbe, "  ret %s\n", op)
	case .Hlt:
		panic("TODO: Hlt not implemented")
	}
}

gen_operand :: proc(qbe: ^QbeCodegen, op: ir.Operand) -> string {
	switch op.kind {
	case .LiteralConst:
		data := op.data.(ir.OperandConstantData)
		return int_to_str(qbe, data.value)
	}
	panic("Unhandled Operand in ir.gen_operand")
}

emit :: proc(qbe: ^QbeCodegen, format: string, args: ..any) {
	fmt.sbprintf(&qbe.sb, format, ..args)
}

int_to_str :: proc(qbe: ^QbeCodegen, val: int) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, qbe.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "%d", val)
	return strings.to_string(sb)
}

type_to_str :: proc(type: ir.TypeKind) -> string {
	switch type {
	case .Word:
		return "w"
	case .Long:
		return "l"
	case .Byte:
		return "b"
	case .HalfByte:
		return "h"
	case .Void:
		panic("Unhandled void type in codegen/qbe.type_to_str")
	}
	panic("Unhandled type in codegen/qbe.type_to_str")
}
