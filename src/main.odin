package nami

import "core:flags"
import vmem "core:mem/virtual"
import "core:os"
import "core:time"

import "ast"
import "codegen"
import qbecodegen "codegen/qbe"
import "fs"
import "ir"
import "logger"
import "parser"
import tc "typechecker"

EXPERIMENTAL_IR :: #config(EXPERIMENTAL_IR, false)

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

	file_contents, ok := fs.read_entire_file(opt.file_name, allocator)
	if !ok {
		logger.error("Error reading file: %s", opt.file_name)
		os.exit(1)
	}

	logger.info("Parsing program")
	p := new(parser.Parser, allocator)
	parser.init(p, file_contents, allocator)
	parser_start := time.now()
	module := parser.parse_module(p)
	if len(p.errors) > 0 {
		for err in p.errors {
			logger.compiler_error(opt.file_name, err)
		}
		os.exit(1)
	}
	logger.info("Parsing complete: %v", time.diff(parser_start, time.now()))

	logger.info("Typechecking program")
	tc_start := time.now()
	typechecker := new(tc.TypeChecker, allocator)
	tc.init(typechecker, module, allocator)
	tc.check_module(typechecker)
	if len(typechecker.errors) != 0 {
		for err in typechecker.errors {
			logger.compiler_error(opt.file_name, err)
		}
		if opt.ast {
			ast.print_ast(module, 0)
		}
		os.exit(1)
	}
	logger.info("Typechecking complete: %v", time.diff(tc_start, time.now()))

	if opt.ast {
		ast.print_ast(module, 0)
		os.exit(0)
	}

	// QBE codegen
	when EXPERIMENTAL_IR {
		{
			logger.info("Generating EXPERIMENTAL_IR")
			ir_start := time.now()
			ctx := ir.new_context(typechecker.symbols[0], allocator)
			ir.from_ast(ctx, module)
			if len(ctx.errors) != 0 {
				for err in ctx.errors {
					logger.compiler_error(opt.file_name, err)
				}
				os.exit(1)
			}
			qbe_codegen := qbecodegen.new_qbecodegen(allocator)
			qbecodegen.from_ir(qbe_codegen, ctx.module)

			logger.info("Starting compilation")
			comp_start := time.now()
			program_name := fs.extract_base_name(opt.file_name)
			ok := codegen.compile_qbe(&qbe_codegen.sb, program_name)
			if !ok {
				os.exit(1)
			}
			logger.info("EXPERIMENTAL_IR complete: %v", time.diff(ir_start, time.now()))
		}
	} else {
		{
			logger.info("Generating QBE")
			qbe_start := time.now()
			qbe: codegen.Qbe
			codegen.qbe_init(&qbe, module, typechecker.symbols[0], allocator)
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
			ok := codegen.compile_qbe(&qbe.sb, program_name)
			if !ok {
				os.exit(1)
			}
			logger.info("Compilation complete: %v", time.diff(comp_start, time.now()))
		}
	}

	logger.info("Finished: Duration: %v", time.diff(start, time.now()))
}
