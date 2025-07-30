package nami

import "core:flags"
import vmem "core:mem/virtual"
import "core:os"
import "core:time"

import "ast"
import "codegen"
import "fs"
import "logger"
import "parser"
import tc "typechecker"

Options :: struct {
	file_name: string `args:"pos=0,required" usage:"Path to file, ex: ./example.nami"`,
	ast:       bool `usage:"Prints the AST"`,
}

main :: proc() {
	start := time.now()
	opt: Options
	style: flags.Parsing_Style = .Unix
	flags.parse_or_exit(&opt, os.args, style)

	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena)
	if arena_err != nil {
		logger.error("Error allocating arena: %v", arena_err)
		os.exit(1)
	}
	allocator := vmem.arena_allocator(&arena)
	defer vmem.arena_free_all(&arena)

	file_contents := fs.read_entire_file(opt.file_name, allocator)
	if len(file_contents) == 0 {
		logger.error("Error reading file: %s", opt.file_name)
		os.exit(1)
	}

	logger.info("Parsing program")
	p := new(parser.Parser, allocator)
	parser.init(p, file_contents, allocator)
	parser_start := time.now()
	program := parser.parse_program(p)
	if len(p.errors) > 0 {
		logger.error("Parser errors:")
		for err in p.errors {
			logger.error("%s:%d:%d: %s", opt.file_name, err.line, err.col, err.msg)
		}
		os.exit(1)
	}
	logger.info("Parsing complete: %v", time.diff(parser_start, time.now()))

	logger.info("Typechecking program")
	tc_start := time.now()
	tchk := new(tc.TypeChecker, allocator)
	tc.init(tchk, program, allocator)
	tc.check_program(tchk)
	if len(tchk.errs) != 0 {
		logger.error("Type errors:")
		for err in tchk.errs {
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
	qbe: codegen.Qbe
	codegen.qbe_init(&qbe, program, tchk.symbols[0], allocator)
	codegen.qbe_generate(&qbe)
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
	program_name := fs.extract_base_name(opt.file_name)
	err := codegen.qbe_compile(&qbe, program_name)
	if err != nil {
		logger.error("Error compiling qbe: %v", err)
		os.exit(1)
	}
	logger.info("Compilation complete: %v", time.diff(comp_start, time.now()))

	logger.info("Finished: Duration: %v", time.diff(start, time.now()))
}
