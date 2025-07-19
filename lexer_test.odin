package main

import vmem "core:mem/virtual"
import "core:testing"

@(test)
test_tokens :: proc(t: ^testing.T) {
	input := `
	fn main() {}
	[]
	"Hello";
	,
	==
	!=
	5
	150
	< >
	+ -
	* / !
	こんにちは
    true
    false
    return
    string
    int
    bool
    void
	`


	expected := []Token {
		Token{type = .FUNC, literal = "fn"},
		Token{type = .IDENT, literal = "main"},
		Token{type = .L_PAREN, literal = "("},
		Token{type = .R_PAREN, literal = ")"},
		Token{type = .L_BRACE, literal = "{"},
		Token{type = .R_BRACE, literal = "}"},
		Token{type = .L_BRACKET, literal = "["},
		Token{type = .R_BRACKET, literal = "]"},
		Token{type = .STRING, literal = "Hello"},
		Token{type = .SEMI_COLON, literal = ";"},
		Token{type = .COMMA, literal = ","},
		Token{type = .EQ, literal = "=="},
		Token{type = .NOT_EQ, literal = "!="},
		Token{type = .INT, literal = "5"},
		Token{type = .INT, literal = "150"},
		Token{type = .LT, literal = "<"},
		Token{type = .GT, literal = ">"},
		Token{type = .PLUS, literal = "+"},
		Token{type = .MINUS, literal = "-"},
		Token{type = .STAR, literal = "*"},
		Token{type = .SLASH, literal = "/"},
		Token{type = .BANG, literal = "!"},
		Token{type = .IDENT, literal = "こんにちは"},
		Token{type = .TRUE, literal = "true"},
		Token{type = .FALSE, literal = "false"},
		Token{type = .RETURN, literal = "return"},
		Token{type = .TYPE_STRING, literal = "string"},
		Token{type = .TYPE_INT, literal = "int"},
		Token{type = .TYPE_BOOL, literal = "bool"},
		Token{type = .TYPE_VOID, literal = "void"},
	}

	lexer: Lexer
	lexer_init(&lexer, input)

	i := 0
	for {
		tok := lexer_next_token(&lexer)
		if tok.type == .EOF {
			break
		}
		if !testing.expectf(
			t,
			is_tok(tok, expected[i]),
			"expected token %v, got %v",
			expected[i],
			tok,
		) {
			break
		}
		i += 1
	}
}

is_tok :: proc(actual: Token, expected: Token) -> bool {
	return actual.type == expected.type && actual.literal == expected.literal
}
