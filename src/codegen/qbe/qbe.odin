package qbe

import "core:fmt"
import "core:mem"
import "core:strings"

import "../../ir"
import "../../logger"

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
	for def in module.data {
		gen_data(qbe, def)
	}
	for t in module.types {
		gen_types(qbe, t)
	}
	for func in module.functions {
		gen_func(qbe, func)
	}
}

gen_types :: proc(qbe: ^QbeCodegen, typedef: ^ir.TypeDef) {
	emit(qbe, "type %s = ", get_aggregate_type_name(qbe, typedef.name))
	if typedef.alignment > 0 {
		emit(qbe, "align %d ", typedef.alignment)
	}
	emit(qbe, "{{ ")
	switch c in typedef.content {
	case ir.UnionTypeContent:
		for field, idx in c.fields {
			emit(qbe, "{{")
			if field.kind == .Aggregate {
				emit(qbe, "%s", get_aggregate_type_name(qbe, typedef.name))
			} else {
				emit(qbe, "%s", type_to_str(field.kind))
			}
			emit(qbe, "}}")
			if idx < len(c.fields) - 1 {
				emit(qbe, ", ")
			}
		}
	case ir.OpaqueTypeContent:
		emit(qbe, "%d", c.size)
	case ir.SequentialTypeContent:
		for field, idx in c.fields {
			if field.kind == .Aggregate {
				name := field.data.(string)
				emit(qbe, "%s", get_aggregate_type_name(qbe, name))
			} else {
				emit(qbe, "%s", type_to_str(field.kind))
				size, ok := field.data.(int)
				if ok {
					emit(qbe, " %d", size)
				}
			}
			if idx < len(c.fields) - 1 {
				emit(qbe, ", ")
			}
		}
	}
	emit(qbe, " }}\n")
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
			assert(f.type != .Void)
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
	if func.linkage == .Export {
		emit(qbe, "export ")
	}
	emit(qbe, "function ")
	if func.return_type != .Void {
		assert(func.return_type != .Void)
		emit(qbe, "%s ", type_to_str(func.return_type))
	}
	emit(qbe, "$%s", func.name)

	emit(qbe, "(")
	for param, idx in func.params {
		assert(param.type != .Void)
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
	emit(qbe, "%s\n", get_label(qbe, block.label))
	for inst in block.instructions {
		gen_inst(qbe, inst)
	}

	if block.terminator != nil {
		gen_jump(qbe, block.terminator)
	}
}

gen_inst :: proc(qbe: ^QbeCodegen, inst: ^ir.Instruction) {
	emit(qbe, "  ")

	// Destination
	dest, ok := inst.dest.?
	if ok {
		emit(qbe, "%s", get_operand(qbe, dest))
		assert(inst.dest_type != nil, "instructions with dest requires a result_type")
		assert(inst.dest_type.? != .Void, fmt.tprintf("%+v", inst))
		emit(qbe, " =%s ", type_to_str(inst.dest_type.?))
	}

	emit(qbe, "%s ", opcode_to_str(qbe, inst))

	if inst.opcode == .Phi {
		assert(inst.phi_sources != nil)
		emit(
			qbe,
			"%s %s, %s %s\n",
			get_label(qbe, inst.phi_sources.src1_label),
			get_operand(qbe, inst.phi_sources.src1_operand),
			get_label(qbe, inst.phi_sources.src2_label),
			get_operand(qbe, inst.phi_sources.src2_operand),
		)
		return
	}

	src1, okk := inst.src1.?
	if okk {
		emit(qbe, "%s", get_operand(qbe, src1))
	}
	if inst.opcode == .Call {
		emit(qbe, "(")
		for arg, idx in inst.call_args {
			if arg.kind == .Variadic {
				emit(qbe, "...")
			} else {
				assert(arg.type != .Void)
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
		data := jump.data.(ir.JnzData)
		emit(
			qbe,
			"  jnz %s, %s, %s\n",
			get_operand(qbe, data.condition),
			get_label(qbe, data.true_label),
			get_label(qbe, data.false_label),
		)
	case .Jmp:
		data := jump.data.(ir.JmpData)
		emit(qbe, "  jmp %s\n", get_label(qbe, data.label))
	case .Ret:
		data := jump.data.(ir.RetData)
		val, ok := data.val.?
		if ok {
			emit(qbe, "  ret %s\n", get_operand(qbe, val))
		} else {
			emit(qbe, "  ret\n")
		}
	case .Hlt:
		panic("TODO: Hlt not implemented")
	}
}

get_aggregate_type_name :: proc(qbe: ^QbeCodegen, name: string) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, qbe.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, ":%s", name)
	return strings.to_string(sb)
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

get_label :: proc(qbe: ^QbeCodegen, label: string) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, qbe.allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "@%s", label)
	return strings.to_string(sb)
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
		return "sle"
	case .SignedGreater:
		return "sgt"
	case .SignedGreaterEqual:
		return "sge"
	case .UnsignedLess:
		return "ult"
	case .UnsignedLessEqual:
		return "ule"
	case .UnsignedGreater:
		return "ugt"
	case .UnsignedGreaterEqual:
		return "uge"
	}
	panic("Unhandled comparison type")
}

opcode_to_str :: proc(qbe: ^QbeCodegen, inst: ^ir.Instruction) -> string {
	switch inst.opcode {
	case .Phi:
		return "phi"
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
		return fmt_str(qbe, "store%s", type_to_str(inst.opcode_type.?))
	case .Load:
		return fmt_str(qbe, "load%s", type_to_str(inst.opcode_type.?))
	case .Alloc:
		return fmt_str(qbe, "alloc%d", inst.alignment)
	case .Call:
		return "call"
	case .Copy:
		return "copy"
	case .Compare:
		return fmt_str(
			qbe,
			"c%s%s",
			comparison_type_to_str(inst.comparison_type),
			type_to_str(inst.opcode_type.?),
		)
	case .Convert:
		return conversion_type_to_str(inst.conversion_type)
	}
	panic("Unhandled opcode in opcode_to_str")
}

conversion_type_to_str :: proc(conversion_type: ir.ConversionType) -> string {
	switch conversion_type {
	case .EXT_SIGNED_WORD:
		return "extsw"
	case .EXT_UNSIGNED_WORD:
		return "extuw"
	case .EXT_SIGNED_HALF:
		return "extsh"
	case .EXT_UNSIGNED_HALF:
		return "extuh"
	case .EXT_SIGNED_BYTE:
		return "extsb"
	case .EXT_UNSIGNED_BYTE:
		return "extub"
	case .EXTEND_SINGLE:
		return "exts"
	case .TRUNCATE_DOUBLE:
		return "truncd"
	case .STOSI:
		return "stosi"
	case .STOUI:
		return "stoui"
	case .DTOSI:
		return "dtosi"
	case .DTOUI:
		return "dtoui"
	case .SWTOF:
		return "swtof"
	case .UWTOF:
		return "uwtof"
	case .SLTOF:
		return "sltof"
	case .ULTOF:
		return "ultof"
	case .None:
		panic("Unexpected conversion type of None")
	}
	panic("Unhandled converstion_type in conversion_type_to_str")
}

fmt_str :: proc(qbe: ^QbeCodegen, ft: string, args: ..any) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, qbe.allocator)
	defer strings.builder_destroy(&sb)

	fmt.sbprintf(&sb, ft, ..args)
	return strings.to_string(sb)
}

type_to_str :: proc(type: ir.TypeKind) -> string {
	switch type {
	case .Word:
		return "w"
	case .Long, .Aggregate:
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
