package lexer

import "core:testing"

import "../token"

@(test)
test_lexer :: proc(t: ^testing.T) {
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
    String
    Int
    Bool
    Void
    get_ident
	if
	else
    &&
    ||
    &
    |
	`


	expected := []token.Token {
		token.Token{type = .FUNC, literal = "fn"},
		token.Token{type = .IDENT, literal = "main"},
		token.Token{type = .L_PAREN, literal = "("},
		token.Token{type = .R_PAREN, literal = ")"},
		token.Token{type = .L_BRACE, literal = "{"},
		token.Token{type = .R_BRACE, literal = "}"},
		token.Token{type = .L_BRACKET, literal = "["},
		token.Token{type = .R_BRACKET, literal = "]"},
		token.Token{type = .STRING, literal = "Hello"},
		token.Token{type = .SEMI_COLON, literal = ";"},
		token.Token{type = .COMMA, literal = ","},
		token.Token{type = .EQ, literal = "=="},
		token.Token{type = .NOT_EQ, literal = "!="},
		token.Token{type = .INT, literal = "5"},
		token.Token{type = .INT, literal = "150"},
		token.Token{type = .LT, literal = "<"},
		token.Token{type = .GT, literal = ">"},
		token.Token{type = .PLUS, literal = "+"},
		token.Token{type = .MINUS, literal = "-"},
		token.Token{type = .STAR, literal = "*"},
		token.Token{type = .SLASH, literal = "/"},
		token.Token{type = .BANG, literal = "!"},
		token.Token{type = .IDENT, literal = "こんにちは"},
		token.Token{type = .TRUE, literal = "true"},
		token.Token{type = .FALSE, literal = "false"},
		token.Token{type = .RETURN, literal = "return"},
		token.Token{type = .TYPE_STRING, literal = "String"},
		token.Token{type = .TYPE_INT, literal = "Int"},
		token.Token{type = .TYPE_BOOL, literal = "Bool"},
		token.Token{type = .TYPE_VOID, literal = "Void"},
		token.Token{type = .IDENT, literal = "get_ident"},
		token.Token{type = .IF, literal = "if"},
		token.Token{type = .ELSE, literal = "else"},
		token.Token{type = .AND, literal = "&&"},
		token.Token{type = .OR, literal = "||"},
		token.Token{type = .AMPERSAND, literal = "&"},
		token.Token{type = .PIPE, literal = "|"},
	}

	lexer: Lexer
	init(&lexer, input)

	i := 0
	for {
		tok := next_token(&lexer)
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

is_tok :: proc(actual: token.Token, expected: token.Token) -> bool {
	return actual.type == expected.type && actual.literal == expected.literal
}
