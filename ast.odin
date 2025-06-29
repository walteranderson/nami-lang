package main

import "core:fmt"
import "core:strings"

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
	func: ^Identifier,
	args: [dynamic]Expr,
}

/////////

Program :: struct {
	stmts: [dynamic]Statement,
}

Function :: struct {
	tok:  Token,
	name: ^Identifier,
	args: [dynamic]^Identifier,
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

ReassignStatement :: struct {
	tok:   Token,
	name:  ^Identifier,
	value: Expr,
}

AssignStatement :: struct {
	tok:   Token,
	name:  ^Identifier,
	value: Expr,
}

///////

Expr :: union {
	^StringLiteral,
	^IntLiteral,
	^Identifier,
	^Boolean,
	^PrefixExpr,
	^InfixExpr,
	^CallExpr,
	^Function,
}

Statement :: union {
	^Program,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
	^AssignStatement,
	^ReassignStatement,
}

Node :: union {
	^StringLiteral,
	^IntLiteral,
	^Identifier,
	^Boolean,
	^PrefixExpr,
	^InfixExpr,
	^CallExpr,
	^Function,
	//
	^Program,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
	^AssignStatement,
	^ReassignStatement,
}

print_ast :: proc(node: Node, indent_level: int) {
	indent := strings.repeat("  ", indent_level)
	switch n in node {
	case ^Program:
		fmt.printf("%sProgram:\n", indent)
		for stmt in n.stmts {
			print_statement(stmt, indent_level + 1)
		}
	case ^ReturnStatement:
		fmt.printf("%sReturnStatement:\n", indent)
		print_expr(n.value, indent_level + 1)
	case ^ExprStatement:
		fmt.printf("%sExprStatement:\n", indent)
		print_expr(n.value, indent_level + 1)
	case ^BlockStatement:
		fmt.printf("%sBlockStatement:\n", indent)
		for stmt in n.stmts {
			print_statement(stmt, indent_level + 1)
		}
	case ^Function:
		fmt.printf("%sFunction: %s\n", indent, n.name.value)
		fmt.printf("%s  Args:", indent)
		if len(n.args) == 0 {
			fmt.printf(" []\n")
		} else {
			fmt.printf("\n")
			for arg in n.args {
				print_ast(cast(Node)arg, indent_level + 2)
			}
		}
		print_ast(cast(Node)n.body, indent_level + 1)
	case ^CallExpr:
		fmt.printf("%sCallExpr: %s\n", indent, n.func.value)
		fmt.printf("%s  Args:\n", indent)
		if len(n.args) == 0 {
			fmt.printf(" []\n")
		} else {
			for arg in n.args {
				print_expr(arg, indent_level + 2)
			}
		}
	case ^InfixExpr:
		fmt.printf("%sInfixExpr: %s\n", indent, n.op)
		print_expr(n.left, indent_level + 1)
		print_expr(n.right, indent_level + 1)
	case ^PrefixExpr:
		fmt.printf("%sPrefixExpr: %s\n", indent, n.op)
		print_expr(n.right, indent_level + 1)
	case ^AssignStatement:
		fmt.printf("%sAssignStatement:\n", indent)
		print_expr(n.name, indent_level + 1)
		print_expr(n.value, indent_level + 1)
	case ^ReassignStatement:
		fmt.printf("%sReassignStatement:\n", indent)
		print_expr(n.name, indent_level + 1)
		print_expr(n.value, indent_level + 1)
	case ^Boolean:
		fmt.printf("%sBoolean: %t\n", indent, n.value)
	case ^Identifier:
		fmt.printf("%sIdentifier: %s\n", indent, n.value)
	case ^IntLiteral:
		fmt.printf("%sIntLiteral: %d\n", indent, n.value)
	case ^StringLiteral:
		fmt.printf("%sStringLiteral: %v\n", indent, n.value)
	}
}

print_statement :: proc(stmt: Statement, indent_level: int) {
	switch s in stmt {
	case ^Program:
		print_ast(cast(Node)s, indent_level)
	case ^ReturnStatement:
		print_ast(cast(Node)s, indent_level)
	case ^ExprStatement:
		print_ast(cast(Node)s, indent_level)
	case ^BlockStatement:
		print_ast(cast(Node)s, indent_level)
	case ^AssignStatement:
		print_ast(cast(Node)s, indent_level)
	case ^ReassignStatement:
		print_ast(cast(Node)s, indent_level)
	}
}

print_expr :: proc(expr: Expr, indent_level: int) {
	switch e in expr {
	case ^StringLiteral:
		print_ast(cast(Node)e, indent_level)
	case ^IntLiteral:
		print_ast(cast(Node)e, indent_level)
	case ^Identifier:
		print_ast(cast(Node)e, indent_level)
	case ^Boolean:
		print_ast(cast(Node)e, indent_level)
	case ^PrefixExpr:
		print_ast(cast(Node)e, indent_level)
	case ^InfixExpr:
		print_ast(cast(Node)e, indent_level)
	case ^CallExpr:
		print_ast(cast(Node)e, indent_level)
	case ^Function:
		print_ast(cast(Node)e, indent_level)
	}
}
