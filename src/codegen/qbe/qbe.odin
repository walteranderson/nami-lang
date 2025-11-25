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
	emit(qbe, "$%s", func.name)

	emit(qbe, "(")
	for param, idx in func.params {
		emit(qbe, "%s %s", type_to_str(param.type), get_operand(qbe, param.op))
		if idx < len(func.params) - 1 {
			emit(qbe, ", ")
		}
	}
	emit(qbe, ") {{\n")

	for block in func.blocks {
		gen_block(qbe, block)
	}
	emit(qbe, "}\n")
}

gen_block :: proc(qbe: ^QbeCodegen, block: ^ir.Block) {
	emit(qbe, "%s\n", new_label(qbe, block.label))
	for inst in block.instructions {
		gen_inst(qbe, inst)
	}
	gen_jump(qbe, block.terminator)
}

gen_inst :: proc(qbe: ^QbeCodegen, inst: ^ir.Instruction) {
	emit(qbe, "  ")

	// Destination
	dest, ok := inst.dest.?
	if ok {
		emit(qbe, "%s", get_operand(qbe, dest))
		assert(inst.dest_type != nil, "instructions with dest requires a result_type")
		emit(qbe, " =%s ", type_to_str(inst.dest_type.?))
	}

	// Opcode
	emit(qbe, "%s", opcode_to_str(inst.opcode))
	if inst.alignment != 0 {
		emit(qbe, "%d", inst.alignment)
	}

	if inst.comparison_type != .None {
		emit(qbe, "%s", comparison_type_to_str(inst.comparison_type))
	}

	if inst.opcode_type != nil {
		emit(qbe, "%s", type_to_str(inst.opcode_type.?))
	}
	emit(qbe, " ")

	// src1, src2
	// $func(...)

	emit(qbe, "%s", get_operand(qbe, inst.src1))
	if inst.opcode == .Call {
		emit(qbe, "(")
		for arg, idx in inst.call_args {
			if arg.kind == .Variadic {
				emit(qbe, "...")
			} else {
				emit(qbe, "%s %s", type_to_str(arg.type), get_operand(qbe, arg.value))
			}
			if idx < len(inst.call_args) - 1 {
				emit(qbe, ", ")
			}
		}
		emit(qbe, ")")
	} else {
		src2, okk := inst.src2.?
		if okk {
			emit(qbe, ", %s", get_operand(qbe, src2))
		}
	}

	emit(qbe, "\n")
}

gen_jump :: proc(qbe: ^QbeCodegen, jump: ^ir.Jump) {
	switch jump.kind {
	case .Jnz:
		panic("TODO: Jnz not implemented")
	case .Jmp:
		panic("TODO: Jmp not implemented")
	case .Ret:
		data := jump.data.(ir.RetData)
		op := get_operand(qbe, data.val)
		emit(qbe, "  ret %s\n", op)
	case .Hlt:
		panic("TODO: Hlt not implemented")
	}
}

get_operand :: proc(qbe: ^QbeCodegen, op: ir.Operand) -> string {
	switch op.kind {
	case .Integer:
		return int_to_str(qbe, op.data.(int))
	case .Temporary:
		sb: strings.Builder
		strings.builder_init(&sb, qbe.allocator)
		defer strings.builder_destroy(&sb)
		fmt.sbprintf(&sb, "%%%s", op.data.(string))
		return strings.to_string(sb)
	case .GlobalSymbol:
		sb: strings.Builder
		strings.builder_init(&sb, qbe.allocator)
		defer strings.builder_destroy(&sb)
		fmt.sbprintf(&sb, "$%s", op.data.(string))
		return strings.to_string(sb)
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

comparison_type_to_str :: proc(type: ir.ComparisonType) -> string {
	switch type {
	case .None:
		return ""
	case .Equal:
		return "eq"
	case .NotEqual:
		return "ne"
	case .SignedLess:
		return "slt"
	case .SignedLessEqual:
		return "slte"
	case .SignedGreater:
		return "sgt"
	case .SignedGreaterEqual:
		return "sgte"
	case .UnsignedLess:
		return "ult"
	case .UnsignedLessEqual:
		return "ulte"
	case .UnsignedGreater:
		return "ugt"
	case .UnsignedGreaterEqual:
		return "ugte"
	}
	panic("Unhandled comparison type")
}

opcode_to_str :: proc(opcode: ir.OpCode) -> string {
	switch opcode {
	case .Add:
		return "add"
	case .Sub:
		return "sub"
	case .Mul:
		return "mul"
	case .Div:
		return "div"
	case .Neg:
		return "neg"
	case .Store:
		return "store"
	case .Load:
		return "load"
	case .Alloc:
		return "alloc"
	case .Call:
		return "call"
	case .Copy:
		return "copy"
	case .Compare:
		return "c"
	}
	panic("Unhandled opcode in opcode_to_str")
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
