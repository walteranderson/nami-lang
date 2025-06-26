package main

StringLiteral :: struct {
	tok:   Token,
	value: string,
}

IntLiteral :: struct {
	tok:   Token,
	value: int,
}

Identifier :: struct {
	tok:   Token,
	value: string,
}

Boolean :: struct {
	tok:   Token,
	value: bool,
}

PrefixExpr :: struct {
	tok:   Token,
	op:    string,
	right: Expr,
}

InfixExpr :: struct {
	tok:   Token,
	left:  Expr,
	op:    string,
	right: Expr,
}

CallExpr :: struct {
	tok:  Token,
	func: Expr,
	args: [dynamic]Expr,
}

/////////

Program :: struct {
	stmts: [dynamic]Statement,
}

Function :: struct {
	tok:  Token,
	args: [dynamic]Expr,
	body: ^BlockStatement,
}

ReturnStatement :: struct {
	tok:   Token,
	value: Expr,
}

ExprStatement :: struct {
	tok:   Token,
	value: Expr,
}

BlockStatement :: struct {
	tok:   Token,
	stmts: [dynamic]Statement,
}

Assignment :: struct {}

///////

Expr :: union {
	^StringLiteral,
	^IntLiteral,
	^Identifier,
	^Boolean,
	^PrefixExpr,
	^InfixExpr,
	^CallExpr,
}

Statement :: union {
	^Program,
	^Function,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
}

Node :: union {
	^StringLiteral,
	^IntLiteral,
	^Identifier,
	^Boolean,
	^PrefixExpr,
	^InfixExpr,
	^CallExpr,
	//
	^Program,
	^Function,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
}
