package main

import "core:fmt"
import "core:mem"
import vmem "core:mem/virtual"
import "core:os"
import "core:strings"

main :: proc() {
	if len(os.args) <= 1 {
		fmt.eprintfln("Usage: nami <input.nami>")
		os.exit(1)
	}

	arena: vmem.Arena
	allocator, arena_err := arena_init(&arena)
	if arena_err != nil {
		fmt.eprintfln("Error allocating arena")
		os.exit(1)
	}
	defer vmem.arena_free_all(&arena)

	file_name := os.args[1]
	file_contents := read_entire_file(file_name, allocator)
	if len(file_contents) == 0 {
		fmt.eprintfln("Error reading file: %s", file_name)
		os.exit(1)
	}

	lexer := new(Lexer, allocator)
	lexer_init(lexer, allocator, file_contents)

	parser := new(Parser, allocator)
	parser_init(parser, lexer, allocator)

	program := parser_parse_program(parser)
	if len(parser.errors) > 0 {
		fmt.eprintln("Parser errors:")
		for err in parser.errors {
			fmt.eprintln("-", err)
		}
		return
	}

	print_ast(program, 0)

	qbe: Qbe
	qbe_init(&qbe, program)

	qbe_generate(&qbe)
	if len(qbe.errors) > 0 {
		fmt.println("QBE codegen errors:")
		for err in qbe.errors {
			fmt.eprintln("-", err)
		}
		return
	}

	program_name := extract_base_name(file_name)
	err := qbe_compile(&qbe, program_name)
	if err != nil {
		fmt.eprintln("Error compiling qbe: %v", err)
		return
	}
}

read_entire_file :: proc(file_name: string, allocator: mem.Allocator) -> string {
	data, ok := os.read_entire_file_from_filename(file_name, allocator)
	if !ok {
		return ""
	}
	return string(data)
}

extract_base_name :: proc(file_name: string) -> string {
	dot_index := strings.last_index_byte(file_name, '.')
	if dot_index != -1 {
		return file_name[:dot_index]
	}
	return file_name
}

create_file_name :: proc(base_name: string, ext: string) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, context.temp_allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "%s.%s", base_name, ext)
	return strings.to_string(sb)
}

arena_init :: proc(arena: ^vmem.Arena) -> (allocator: mem.Allocator, err: mem.Allocator_Error) {
	vmem.arena_init_growing(arena) or_return
	allocator = vmem.arena_allocator(arena)
	return
}
