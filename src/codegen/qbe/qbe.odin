package qbe

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
	//
}
