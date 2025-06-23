package main

import "core:fmt"
import vmem "core:mem/virtual"
import "core:os"
import "core:testing"

@(test)
test_main_func :: proc(t: ^testing.T) {
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena)
	if arena_err != nil {
		fmt.eprintfln("Error allocating arena")
		os.exit(1)
	}
	defer vmem.arena_free_all(&arena)
	allocator := vmem.arena_allocator(&arena)

	l: Lexer
	lexer_init(&l, allocator, "fn main() {}")

	p: Parser
	parser_init(&p, &l, allocator)

	program := parser_parse(&p)
	testing.expect(t, len(p.errors) == 0, "Parser had errors")
}

@(test)
test_no_main_func :: proc(t: ^testing.T) {
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena)
	if arena_err != nil {
		fmt.eprintfln("Error allocating arena")
		os.exit(1)
	}
	defer vmem.arena_free_all(&arena)
	allocator := vmem.arena_allocator(&arena)

	l: Lexer
	lexer_init(&l, allocator, "fn foo() {}")

	p: Parser
	parser_init(&p, &l, allocator)

	program := parser_parse(&p)
	missing_err := "Missing main function"
	testing.expectf(
		t,
		p.errors[0] == missing_err,
		"expected error \"%s\", got \"%s\"\n",
		missing_err,
		p.errors[0],
	)
}
