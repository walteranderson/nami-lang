package ir

import "core:mem"

Context :: struct {
	allocator:     mem.Allocator,
	next_reg_id:   int,
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
	reg_id: int,
	type:   TypeKind,
}

Block :: struct {
	reg_id:       int,
	label:        string,
	instructions: [dynamic]^Instruction,
}

Instruction :: struct {
	opcode: OpCode,
	dest:   Operand,
	src1:   Operand,
	src2:   Operand,
}

Operand :: struct {}

OpCode :: enum {
	Add,
	// Sub,
	// Mul,
	// Div,
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
