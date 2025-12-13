package ir

import "core:mem"

import "../logger"

Module :: struct {
	functions: [dynamic]^FunctionDef,
	data:      [dynamic]^DataDef,
	types:     [dynamic]^TypeDef,
}

FunctionDef :: struct {
	linkage:     Linkage,
	return_type: TypeKind,
	name:        string,
	params:      [dynamic]FunctionParam,
	blocks:      [dynamic]^Block,
}

FunctionParam :: struct {
	type:           TypeKind,
	op:             Operand,

	// TODO: more comprehensive solution to aggregate type lookups
	aggregate_name: string,
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
	val: Maybe(Operand),
}

Instruction :: struct {
	dest:            Maybe(Operand),
	dest_type:       Maybe(TypeKind),
	opcode:          OpCode,
	opcode_type:     Maybe(TypeKind),
	src1:            Maybe(Operand),
	src2:            Maybe(Operand),

	// optional metadata depending on the opcode
	alignment:       int,
	comparison_type: ComparisonType,
	conversion_type: ConversionType,
	call_args:       [dynamic]CallArgument,
	phi_sources:     ^PhiSources,
}

PhiSources :: struct {
	src1_label:   string,
	src1_operand: Operand,
	src2_label:   string,
	src2_operand: Operand,
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
	Convert,
	// memory
	Store,
	Load,
	Alloc,
	// control & metadata
	Call,
	Copy,
	Compare,
	Phi,
}


ConversionType :: enum {
	None,
	// 1. Integer Precision Extension (Sub-Word/Word to Long/Word)
	EXT_SIGNED_WORD, // extsw (w -> l)
	EXT_UNSIGNED_WORD, // extuw (w -> l)
	EXT_SIGNED_HALF, // extsh (h -> w/l)
	EXT_UNSIGNED_HALF, // extuh (h -> w/l)
	EXT_SIGNED_BYTE, // extsb (b -> w/l)
	EXT_UNSIGNED_BYTE, // extub (b -> w/l)

	// 2. Floating-Point Precision Change (d <-> s)
	EXTEND_SINGLE, // exts (s -> d)
	TRUNCATE_DOUBLE, // truncd (d -> s)

	// 3. Floating-Point to Integer Conversion (Float -> Integer)
	STOSI, // stosi (single to signed integer)
	STOUI, // stoui (single to unsigned integer)
	DTOSI, // dtosi (double to signed integer)
	DTOUI, // dtoui (double to unsigned integer)

	// 4. Integer to Floating-Point Conversion (Integer -> Float)
	SWTOF, // swtof (signed word to float)
	UWTOF, // uwtof (unsigned word to float)
	SLTOF, // sltof (signed long to float)
	ULTOF, // ultof (unsigned long to float)
}

ComparisonType :: enum {
	None,
	Equal,
	NotEqual,
	SignedLess,
	SignedLessEqual,
	SignedGreater,
	SignedGreaterEqual,
	UnsignedLess,
	UnsignedLessEqual,
	UnsignedGreater,
	UnsignedGreaterEqual,
}

CallArgument :: struct {
	kind:           enum {
		Regular,
		Variadic,
	},
	type:           TypeKind,
	value:          Operand,

	// TODO: more comprehensive solution to aggregate type lookups
	aggregate_name: string,
}


TypeDef :: struct {
	name:      string,
	alignment: int,
	content:   TypeDefContent,
}

TypeDefContent :: union {
	OpaqueTypeContent,
	SequentialTypeContent,
	UnionTypeContent,
}

// type :t = align 16 { 32 }
// alignment is required
OpaqueTypeContent :: struct {
	size: int,
}

// type :t = { w 32, l, :foo }
SequentialTypeContent :: struct {
	fields: [dynamic]TypeField,
}

// type :t = { {w}, {l}, {:foo} }
UnionTypeContent :: struct {
	fields: [dynamic]TypeField,
}

TypeField :: struct {
	kind: TypeKind,
	data: union {
		string, // for ident
		int, // size
	},
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
	Aggregate,
}
