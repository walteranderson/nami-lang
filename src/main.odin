package nami

import "core:flags"
import "core:fmt"
import "core:mem"
import vmem "core:mem/virtual"
import "core:os"
import "core:strings"
import "core:time"

import "ast"
import "logger"

Options :: struct {
	file_name: string `args:"pos=0,required" usage:"Path to file, ex: ./example.nami"`,
	ast:       bool `usage:"Prints the AST"`,
	tokens:    bool `usage:"Prints out tokens"`,
}

main :: proc() {
	start := time.now()
	opt: Options
	style: flags.Parsing_Style = .Unix
	flags.parse_or_exit(&opt, os.args, style)

	arena: vmem.Arena
	allocator, arena_err := arena_init(&arena)
	if arena_err != nil {
		logger.error("Error allocating arena: %v", arena_err)
		os.exit(1)
	}
	defer vmem.arena_free_all(&arena)

	file_contents := read_entire_file(opt.file_name, allocator)
	if len(file_contents) == 0 {
		logger.error("Error reading file: %s", opt.file_name)
		os.exit(1)
	}

	parser := new(Parser, allocator)
	parser_init(parser, file_contents, allocator)

	if opt.tokens {
		print_tokens(parser, opt.file_name)
		os.exit(0)
	}

	logger.info("Parsing program")
	parser_start := time.now()
	program := parser_parse_program(parser)
	if len(parser.errors) > 0 {
		logger.error("Parser errors:")
		for err in parser.errors {
			logger.error("%s:%d:%d: %s", opt.file_name, err.line, err.col, err.msg)
		}
		os.exit(1)
	}
	logger.info("Parsing complete: %v", time.diff(parser_start, time.now()))

	logger.info("Typechecking program")
	tc_start := time.now()
	tc := new(TypeChecker, allocator)
	tc_init(tc, program, allocator)
	tc_check_program(tc)
	if len(tc.errs) != 0 {
		logger.error("Type errors:")
		for err in tc.errs {
			logger.error(err)
		}
		if opt.ast {
			ast.print_ast(program, 0)
		}
		os.exit(1)
	}
	logger.info("Typechecking complete: %v", time.diff(tc_start, time.now()))

	if opt.ast {
		ast.print_ast(program, 0)
		os.exit(0)
	}

	logger.info("Generating QBE")
	qbe_start := time.now()
	qbe: Qbe
	qbe_init(&qbe, program, tc.symbols[0], allocator)
	qbe_generate(&qbe)
	if len(qbe.errors) > 0 {
		logger.error("QBE codegen errors:")
		for err in qbe.errors {
			logger.error(err)
		}
		os.exit(1)
	}
	logger.info("Codegen complete: %v", time.diff(qbe_start, time.now()))

	logger.info("Starting compilation")
	comp_start := time.now()
	program_name := extract_base_name(opt.file_name)
	err := qbe_compile(&qbe, program_name)
	if err != nil {
		logger.error("Error compiling qbe: %v", err)
		os.exit(1)
	}
	logger.info("Compilation complete: %v", time.diff(comp_start, time.now()))

	logger.info("Finished: Duration: %v", time.diff(start, time.now()))
}

print_tokens :: proc(p: ^Parser, file_name: string) {
	for !parser_cur_token_is(p, .EOF) {
		logger.info(
			"%s:%d:%d - type=%s, literal=%s",
			file_name,
			p.cur.line,
			p.cur.col,
			p.cur.type,
			p.cur.literal,
		)
		parser_next_token(p)
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
