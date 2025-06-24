package main

import "core:fmt"
import vmem "core:mem/virtual"
import "core:os"
import "core:testing"

@(test)
test_main_func :: proc(t: ^testing.T) {
	arena: vmem.Arena
	allocator, err := arena_init(&arena)
	testing.expect(t, err == nil, "Error allocating arena")
	defer vmem.arena_free_all(&arena)

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
	allocator, err := arena_init(&arena)
	testing.expect(t, err == nil, "Error allocating arena")
	defer vmem.arena_free_all(&arena)

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
