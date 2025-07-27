package ast

import "core:fmt"
import "core:strings"

import t "../token"

Node :: struct {
	tok:           t.Token,
	resolved_type: ^TypeInfo,
}

StringLiteral :: struct {
	using node: Node,
	value:      string,
}

IntLiteral :: struct {
	using node: Node,
	value:      int,
}

Identifier :: struct {
	using node: Node,
	value:      string,
}

Boolean :: struct {
	using node: Node,
	value:      bool,
}

PrefixExpr :: struct {
	using node: Node,
	op:         string, // ! or -
	right:      Expr,
}

InfixExpr :: struct {
	using node: Node,
	left:       Expr,
	op:         string,
	right:      Expr,
}

CallExpr :: struct {
	using node: Node,
	func:       ^Identifier,
	args:       [dynamic]Expr,
}

IfStatement :: struct {
	using node:  Node,
	condition:   Expr,
	consequence: ^BlockStatement,
	alternative: ^BlockStatement,
}

/////////

Program :: struct {
	using node: Node,
	stmts:      [dynamic]Statement,
}

FunctionStatement :: struct {
	using node:           Node,
	name:                 ^Identifier,
	args:                 [dynamic]^FunctionArg,
	body:                 ^BlockStatement,
	declared_return_type: ^TypeAnnotation,
}

FunctionArg :: struct {
	using node:    Node,
	ident:         ^Identifier,
	declared_type: ^TypeAnnotation,
}

ReturnStatement :: struct {
	using node: Node,
	value:      Expr,
}

ExprStatement :: struct {
	using node: Node,
	value:      Expr,
}

BlockStatement :: struct {
	using node: Node,
	stmts:      [dynamic]Statement,
}

ReassignStatement :: struct {
	using node: Node,
	name:       ^Identifier,
	value:      Expr,
}

AssignStatement :: struct {
	using node:    Node,
	name:          ^Identifier,
	value:         Expr, // optional
	declared_type: ^TypeAnnotation,
}

///////

TypeKind :: enum {
	Invalid,
	Void,
	Any,
	Bool,
	Int,
	String,
	Function,
}

FunctionTypeInfo :: struct {
	param_types: [dynamic]^TypeInfo,
	return_type: ^TypeInfo,
}

TypeInfo :: struct {
	kind: TypeKind,
	data: union {
		FunctionTypeInfo,
	},
}

TypeAnnotation :: struct {
	tok:  t.Token,
	name: string,
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
}

Statement :: union {
	^Program,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
	^AssignStatement,
	^ReassignStatement,
	^FunctionStatement,
	^FunctionArg,
	^IfStatement,
}

AnyNode :: union {
	^StringLiteral,
	^IntLiteral,
	^Identifier,
	^Boolean,
	^PrefixExpr,
	^InfixExpr,
	^CallExpr,
	//
	^Program,
	^ReturnStatement,
	^ExprStatement,
	^BlockStatement,
	^AssignStatement,
	^ReassignStatement,
	^FunctionStatement,
	^FunctionArg,
	^IfStatement,
}

print_ast :: proc(node: AnyNode, indent_level: int) {
	indent := strings.repeat("  ", indent_level)
	switch n in node {
	case ^Program:
		fmt.printf("%sProgram:\n", indent)
		for stmt in n.stmts {
			print_statement(stmt, indent_level + 1)
		}
	case ^ReturnStatement:
		fmt.printf("%sReturnStatement:\n", indent)
		fmt.printf("%s  ResolvedType: %s\n", indent, n.resolved_type.kind)
		print_expr(n.value, indent_level + 1)
	case ^ExprStatement:
		fmt.printf("%sExprStatement:\n", indent)
		print_expr(n.value, indent_level + 1)
	case ^BlockStatement:
		fmt.printf("%sBlockStatement:\n", indent)
		for stmt in n.stmts {
			print_statement(stmt, indent_level + 1)
		}
	case ^FunctionStatement:
		fmt.printf("%sFunctionStatement: %s\n", indent, n.name.value)
		fmt.printf("%s  Args:", indent)
		if len(n.args) == 0 {
			fmt.printf(" []\n")
		} else {
			fmt.printf("\n")
			for arg in n.args {
				print_ast(cast(AnyNode)arg, indent_level + 2)
			}
		}
		if n.declared_return_type != nil {
			fmt.printf("%s  DeclaredReturnType: %s\n", indent, n.declared_return_type.name)
		}
		typeinfo := n.resolved_type.data.(FunctionTypeInfo)
		fmt.printf("%s  ResolvedReturnType: %s\n", indent, typeinfo.return_type.kind)
		print_ast(cast(AnyNode)n.body, indent_level + 1)
	case ^FunctionArg:
		fmt.printf("%sFunctionArg: %s\n", indent, n.ident.value)
		if n.declared_type != nil {
			fmt.printf("%s  Type: %s\n", indent, n.declared_type.name)
		}
	case ^CallExpr:
		fmt.printf("%sCallExpr: %s\n", indent, n.func.value)
		fmt.printf("%s  Args:", indent)
		if len(n.args) == 0 {
			fmt.printf(" []\n")
		} else {
			fmt.printf("\n")
			for arg in n.args {
				print_expr(arg, indent_level + 2)
			}
		}
	case ^InfixExpr:
		fmt.printf("%sInfixExpr: %s\n", indent, n.op)
		fmt.printf("%s  ResolvedType: %s\n", indent, n.resolved_type.kind)
		print_expr(n.left, indent_level + 1)
		print_expr(n.right, indent_level + 1)
	case ^PrefixExpr:
		fmt.printf("%sPrefixExpr: %s\n", indent, n.op)
		fmt.printf("%s  ResolvedType: %s\n", indent, n.resolved_type.kind)
		print_expr(n.right, indent_level + 1)
	case ^AssignStatement:
		fmt.printf("%sAssignStatement:\n", indent)
		print_expr(n.name, indent_level + 1)
		if n.declared_type != nil {
			fmt.printf("%s  DeclaredType: %s\n", indent, n.declared_type.name)
		}
		fmt.printf("%s  ResolvedType: %s\n", indent, n.resolved_type.kind)
		print_expr(n.value, indent_level + 1)
	case ^ReassignStatement:
		fmt.printf("%sReassignStatement:\n", indent)
		fmt.printf("%s  ResolvedType: %s\n", indent, n.resolved_type.kind)
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
	case ^IfStatement:
		fmt.printf("%sIf Statement:\n", indent)
		fmt.printf("%s  Condition:\n", indent)
		print_expr(n.condition, indent_level + 2)
		fmt.printf("%s  Consequence:\n", indent)
		print_statement(n.consequence, indent_level + 2)
		if n.alternative != nil {
			fmt.printf("%s  Alternative:\n", indent)
			print_statement(n.alternative, indent_level + 2)
		}
	}
}

print_statement :: proc(stmt: Statement, indent_level: int) {
	switch s in stmt {
	case ^Program:
		print_ast(cast(AnyNode)s, indent_level)
	case ^ReturnStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^ExprStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^BlockStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^AssignStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^ReassignStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^FunctionStatement:
		print_ast(cast(AnyNode)s, indent_level)
	case ^FunctionArg:
		print_ast(cast(AnyNode)s, indent_level)
	case ^IfStatement:
		print_ast(cast(AnyNode)s, indent_level)
	}
}

print_expr :: proc(expr: Expr, indent_level: int) {
	switch e in expr {
	case ^StringLiteral:
		print_ast(cast(AnyNode)e, indent_level)
	case ^IntLiteral:
		print_ast(cast(AnyNode)e, indent_level)
	case ^Identifier:
		print_ast(cast(AnyNode)e, indent_level)
	case ^Boolean:
		print_ast(cast(AnyNode)e, indent_level)
	case ^PrefixExpr:
		print_ast(cast(AnyNode)e, indent_level)
	case ^InfixExpr:
		print_ast(cast(AnyNode)e, indent_level)
	case ^CallExpr:
		print_ast(cast(AnyNode)e, indent_level)
	}
}
