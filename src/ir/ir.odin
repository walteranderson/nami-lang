package ir

import "core:mem"

Context :: struct {
	allocator:     mem.Allocator,
	module:        ^Module,
	next_tmp_id:   int,
	next_label_id: int,
}

Module :: struct {
	functions: [dynamic]^FunctionDef,
	// data:      [dynamic]^DataDef,
	// types:     [dynamic]^TypeDef,
}

FunctionDef :: struct {
	linkage:     Linkage,
	return_type: TypeKind,
	name:        string,
	params:      [dynamic]FunctionParam,
	blocks:      [dynamic]^Block,
}

FunctionParam :: struct {
	tmp_id: int,
	type:   TypeKind,
}

Block :: struct {
	label:        string,
	instructions: [dynamic]^Instruction,
	terminator:   ^Jump,
}

Jump :: struct {
	kind: JumpType,
	data: union {
		JmpData,
		JnzData,
		RetData,
	},
}

JumpType :: enum {
	Jmp,
	Jnz,
	Ret,
	Hlt,
}

JmpData :: struct {
	label: string,
}

JnzData :: struct {
	condition:   Operand,
	true_label:  string,
	false_label: string,
}

RetData :: struct {
	val: Operand,
}

Instruction :: struct {
	return_type: TypeKind,
	opcode:      OpCode,
	dest:        Operand,
	src1:        Operand,
	src2:        Operand,
}

Operand :: struct {
	kind: OperandKind,
	// explicit_type: ...
	data: union {
		OperandConstantData,
	},
}

OperandKind :: enum {
	LiteralConst,
}

OperandConstantData :: struct {
	value: int,
}

OpCode :: enum {
	Add,
	Sub,
	Mul,
	Div,
	Call,
	// Alloc,
}

DataDef :: struct {
	name:    string,
	linkage: Linkage,
	type:    TypeKind,
	value:   union {
		int,
		string,
		bool,
	},
}

Linkage :: enum {
	None,
	Export,
	// Thread,
	// Section<name, flags>
}

TypeKind :: enum {
	Void,
	Word,
	Long,
	// TODO: floats
	// Single,
	// Double,
	Byte,
	HalfByte,
}
