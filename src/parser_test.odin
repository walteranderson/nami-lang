package nami

import vmem "core:mem/virtual"
import "core:testing"

@(test)
test_parser :: proc(t: ^testing.T) {
	input := `
    fn main() int {
        foo := 34 + 35;
        printf("%d\n", foo);
        return 0;
    }
    `


	arena: vmem.Arena
	allocator, arena_err := arena_init(&arena)
	if arena_err != nil {
		return
	}
	defer vmem.arena_free_all(&arena)

	parser: Parser
	parser_init(&parser, input, allocator)

	program := parser_parse_program(&parser)
	testing.expectf(t, len(parser.errors) == 0, "expected no errors")
	testing.expect(t, len(program.stmts) > 0, "expected more than one global statement")

	fn, ok := program.stmts[0].(^FunctionStatement)
	testing.expect(t, ok, "expected function")
	testing.expect(t, len(fn.args) == 0, "expected 0 args")
	testing.expect(t, len(fn.body.stmts) > 0, "expected to have function body statements")

	{
		assign, ok1 := fn.body.stmts[0].(^AssignStatement)
		testing.expect(t, ok1, "expected first statement to be an assignment")

		infix, ok2 := assign.value.(^InfixExpr)
		testing.expect(t, ok2, "expected expression to be infix")

		left, ok3 := infix.left.(^IntLiteral)
		testing.expect(t, ok3, "expected int literal for left")
		testing.expectf(t, left.value == 34, "expected left to be 34, got %d", left.value)

		right, ok4 := infix.right.(^IntLiteral)
		testing.expect(t, ok4, "expected int literal for right")
		testing.expectf(t, right.value == 35, "expected right to be 35, got %d", right.value)
	}

	testing.expect(t, len(fn.body.stmts) > 2, "expected to have at least 2 statements")

	expr_stmt, ok5 := fn.body.stmts[1].(^ExprStatement)
	testing.expect(t, ok5, "expected expression statement")

	call_expr, ok6 := expr_stmt.value.(^CallExpr)
	testing.expect(t, ok6, "expected call expression")

	testing.expectf(
		t,
		call_expr.func.value == "printf",
		"expected printf function ident, got %s",
		call_expr.func.value,
	)
	testing.expect(t, len(call_expr.args) == 2, "expected 2 args to call expr")

	fmt, ok7 := call_expr.args[0].(^StringLiteral)
	testing.expect(t, ok7, "expected string literal for first call expr arg")
	testing.expectf(t, fmt.value == "%d\\n", "expected printf format string, got %s", fmt.value)

	ident, ok8 := call_expr.args[1].(^Identifier)
	testing.expect(t, ok8, "expected identifier for second call expr arg")
	testing.expectf(t, ident.value == "foo", "expected ident foo, got %s", ident.value)
}
