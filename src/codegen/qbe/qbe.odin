package qbe

import "core:fmt"
import "core:mem"
import "core:strings"

import "../../ir"

QbeCodegen :: struct {
	allocator:     mem.Allocator,
	sb:            strings.Builder,
	next_label_id: int,
	next_tmp_id:   int,
}

new_qbecodegen :: proc(allocator: mem.Allocator) -> ^QbeCodegen {
	qbe := new(QbeCodegen, allocator)
	qbe.allocator = allocator
	strings.builder_init(&qbe.sb, allocator)
	return qbe
}

from_ir :: proc(qbe: ^QbeCodegen, module: ^ir.Module) {
	for def in module.data {
		gen_data(qbe, def)
	}
	for func in module.functions {
		gen_func(qbe, func)
	}
}

gen_data :: proc(qbe: ^QbeCodegen, def: ^ir.DataDef) {
	if def.linkage == .Export {
		emit(qbe, "export ")
	}
	emit(qbe, "data $%s = {{ ", def.name)
	for field, idx in def.content.fields {
		switch f in field {
		case ir.DataZeroInit:
			emit(qbe, "z %d", f.size)
		case ir.DataInit:
			emit(qbe, "%s ", type_to_str(f.type))
			for item in f.items {
				switch i in item {
				case int:
					emit(qbe, "%d", i)
				case string:
					emit(qbe, "\"%s\"", i)
				}
			}
		}
		if idx < len(def.content.fields) - 1 {
			emit(qbe, ", ")
		}
	}
	emit(qbe, " }}\n")
}

gen_func :: proc(qbe: ^QbeCodegen, func: ^ir.FunctionDef) {
	qbe.next_label_id = 0
	qbe.next_tmp_id = 0
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
	emit(qbe, "%s\n", new_label(qbe, block.label))
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
	case .Integer:
		return int_to_str(qbe, op.data.(int))
	case .Temporary:
	// TODO
	case .GlobalSymbol:
	// TODO
	case .Invalid:
		panic("Invalid operand")
	}
	panic("Unhandled Operand in ir.gen_operand")
}

emit :: proc(qbe: ^QbeCodegen, format: string, args: ..any) {
	fmt.sbprintf(&qbe.sb, format, ..args)
}

new_label :: proc(qbe: ^QbeCodegen, name: string) -> string {
	qbe.next_label_id += 1
	sb: strings.Builder
	strings.builder_init(&sb, qbe.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "@%s.%d", name, qbe.next_label_id)
	return strings.to_string(sb)
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
