package ir

import "core:mem"

import "../logger"

Module :: struct {
	functions: [dynamic]^FunctionDef,
	data:      [dynamic]^DataDef,
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
	type: TypeKind,
	op:   Operand,
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
	dest:        Maybe(Operand),
	opcode:      OpCode,
	src1:        Operand,
	src2:        Maybe(Operand),
	alignment:   int,
	result_type: Maybe(TypeKind),
}

Operand :: struct {
	kind: OperandKind,
	// explicit_type: ...
	data: union {
		// for Temporary, and GlobalSymbol
		string,
		// for Integer
		int,
	},
}

OperandKind :: enum {
	Invalid,
	Integer,
	Temporary,
	GlobalSymbol,
}

OpCode :: enum {
	// arithmetic
	Add,
	Sub,
	Mul,
	Div,
	Neg,
	// memory
	Store,
	Load,
	Alloc,
	// control & metadata
	Call,
	Copy,
	// TODO: handle these are a generic "Comparison" with the variation being in some kind of metadata
	Ceq,
}

DataDef :: struct {
	name:    string,
	linkage: Linkage,
	content: DataContent,
}

DataContent :: struct {
	fields: [dynamic]DataField,
}

DataField :: union {
	DataZeroInit,
	DataInit,
}

DataZeroInit :: struct {
	size: int,
}

DataInit :: struct {
	type:  TypeKind,
	items: [dynamic]DataItem,
}

DataItem :: union {
	string,
	int,
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
