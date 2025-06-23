package main

import "core:fmt"
import os "core:os/os2"
import "core:strings"

Qbe :: struct {
	sb:         strings.Builder,
	program:    ^Program,
	errors:     [dynamic]string,
	strs:       map[string]string,
	strs_count: int,
}

qbe_init :: proc(qbe: ^Qbe, program: ^Program) {
	strings.builder_init(&qbe.sb)
	qbe.program = program
}

qbe_generate :: proc(qbe: ^Qbe) {
	for stmt in qbe.program.stmts {
		qbe_gen_stmt(qbe, stmt)
	}

	if len(qbe.strs) > 0 {
		for content, label in qbe.strs {
			qbe_emit(qbe, "data %s = {{ b \"%s\" }}\n", label, content)
		}
	}
}

qbe_gen_stmt :: proc(qbe: ^Qbe, stmt: Stmt) {
	if s, ok := stmt.(^Function); ok {
		qbe_gen_func(qbe, s)
		return
	}
	if s, ok := stmt.(^ExprStmt); ok {
		qbe_gen_expr(qbe, s.expr)
		return
	}
	qbe_error(qbe, "unexpected statement: %v", stmt)
}

qbe_gen_func :: proc(qbe: ^Qbe, stmt: ^Function) {
	if stmt.name == "main" {
		qbe_emit(qbe, "export ")
	}
	fmt.sbprintf(&qbe.sb, "function w $%s() {{\n", stmt.name)
	qbe_emit(qbe, "@start\n")

	for stmt in stmt.body.stmts {
		qbe_emit(qbe, "\t")
		qbe_gen_stmt(qbe, stmt)
	}

	qbe_emit(qbe, "\tret 0\n")
	qbe_emit(qbe, "}}\n")
}

qbe_gen_expr :: proc(qbe: ^Qbe, expr: Expr) {
	switch v in expr {
	case ^CallExpr:
		qbe_gen_call_expr(qbe, v)
	case ^StringLiteral:
		qbe_check_string_literal(qbe, v)
	}
}

qbe_check_string_literal :: proc(qbe: ^Qbe, s: ^StringLiteral) -> string {
	if existing, found := qbe.strs[s.value]; found {
		return existing
	}

	label := fmt.aprintf("$str_%d", qbe.strs_count)
	qbe.strs[s.value] = label
	qbe.strs_count += 1
	return label
}

qbe_gen_call_expr :: proc(qbe: ^Qbe, e: ^CallExpr) {
	if e.callee == "print" {
		labels: [dynamic]string
		for arg in e.args {
			if s, ok := arg.(^StringLiteral); ok {
				label := qbe_check_string_literal(qbe, s)
				append(&labels, label)
			} else {
				qbe_error(qbe, "unsupported call expression argument, got %s", arg)
				break
			}
		}
		qbe_emit(qbe, "call $printf(")
		for l, i in labels {
			qbe_emit(qbe, "l %s", l)
			if i + 1 < len(labels) {
				qbe_emit(qbe, ", ")
			}
		}
		qbe_emit(qbe, ")\n")
	} else {
		qbe_error(qbe, "unsupported call expression, got %s", e.callee)
	}
}

qbe_emit :: proc(qbe: ^Qbe, format: string, args: ..any) {
	fmt.sbprintf(&qbe.sb, format, ..args)
}

qbe_error :: proc(qbe: ^Qbe, ft: string, args: ..any) {
	append(&qbe.errors, fmt.tprintf(ft, ..args))
}

qbe_compile :: proc(qbe: ^Qbe, program_name: string) -> (err: os.Error) {
	qbe_file := create_file_name(program_name, "ssa")
	asm_file := create_file_name(program_name, "s")

	content := strings.to_string(qbe.sb)
	os.write_entire_file(qbe_file, transmute([]byte)(content)) or_return

	qbe_cmd := []string{"qbe", "-o", asm_file, qbe_file}
	fmt.printfln("CMD: %v", qbe_cmd)

	qbe_desc := os.Process_Desc {
		command = qbe_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(qbe_desc) or_return

	cc_cmd := []string{"cc", "-o", program_name, asm_file}
	fmt.printfln("CMD: %v", cc_cmd)

	cc_desc := os.Process_Desc {
		command = cc_cmd,
		stdin   = os.stdin,
		stdout  = os.stdout,
		stderr  = os.stderr,
	}
	_ = os.process_start(cc_desc) or_return

	// p := os.process_start(
	// 	{command = {"qbe", "-o", asm_file, " ", qbe_file}, stdout = os.stdout, stderr = os.stderr},
	// ) or_return
	// _ = os.process_wait(p) or_return

	// p = os.process_start(
	// 	{
	// 		command = {"cc", "-o", program_name, " ", asm_file},
	// 		stdout = os.stdout,
	// 		stderr = os.stderr,
	// 	},
	// ) or_return
	// _ = os.process_wait(p) or_return

	return nil
}
