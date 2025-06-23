package main

import "core:mem"

Node :: struct {
	tok:     Token,
	derived: AnyNode,
}

Program :: struct {
	using node: Node,
	stmts:      [dynamic]Stmt,
}

Function :: struct {
	using node: Node,
	name:       string,
	// params
	body:       ^BlockStmt,
}

BlockStmt :: struct {
	using node: Node,
	stmts:      [dynamic]Stmt,
}

CallExpr :: struct {
	using node: Node,
	callee:     string,
	args:       [dynamic]Expr,
}

StringLiteral :: struct {
	using node: Node,
	value:      string,
}

ExprStmt :: struct {
	using node: Node,
	expr:       Expr,
}

AnyNode :: union {
	// root
	^Program,
	// statements
	^BlockStmt,
	^Function,
	^ExprStmt,
	// expressions
	^CallExpr,
	^StringLiteral,
}

Stmt :: union {
	^BlockStmt,
	^Function,
	^ExprStmt,
}

Expr :: union {
	^CallExpr,
	^StringLiteral,
}


ast_new :: proc($T: typeid, tok: Token, allocator: mem.Allocator) -> ^T {
	n, _ := new(T, allocator)
	n.derived = n
	n.tok = tok
	return n
}
