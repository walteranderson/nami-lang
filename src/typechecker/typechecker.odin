package typechecker

import "core:fmt"
import "core:mem"
import "core:strings"

import "../ast"
import "../logger"
import "../token"

TypeChecker :: struct {
	errors:    [dynamic]logger.CompilerError,
	allocator: mem.Allocator,
	program:   ^ast.Module,
	symbols:   [dynamic]map[string]^ast.TypeInfo,
	has_main:  bool,
}

init :: proc(tc: ^TypeChecker, p: ^ast.Module, allocator: mem.Allocator) {
	tc.program = p
	tc.allocator = allocator
	tc.errors = make([dynamic]logger.CompilerError, allocator)
	symbols_push_scope(tc)
}

check_module :: proc(tc: ^TypeChecker) {
	register_symbols(tc)
	if len(tc.errors) > 0 {
		return
	}

	for stmt in tc.program.stmts {
		check_stmt(tc, stmt, expected_return_type = .Invalid)
	}

	if !tc.has_main {
		error(tc, tc.program.tok, "Missing main function")
	}
}

register_symbols :: proc(tc: ^TypeChecker) {
	for stmt in tc.program.stmts {
		#partial switch s in stmt {
		case ^ast.FunctionStatement:
			check_func_signature(tc, s)
		case ^ast.StructStatement:
		// TODO: QBE doesn't let you define types in whatever order you want.
		// So until i decide to topological sort the types during ir generation, structs have to be defined before use
		// register_struct_symbol(tc, s)
		}
	}
}

register_struct_symbol :: proc(tc: ^TypeChecker, stmt: ^ast.StructStatement) {
	_, found := lookup_symbol(tc, stmt.name.value)
	if found {
		error(
			tc,
			stmt.name.tok,
			"Struct already declared: %s",
			stmt.name.value,
		)
		return
	}

	typeinfo := make_typeinfo(tc, .Struct)
	typeinfo.data = ast.StructTypeInfo {
		name = stmt.name.value,
	}
	stmt.resolved_type = typeinfo
	add_symbol(tc, stmt.name.value, stmt.resolved_type)
}

check_func_signature :: proc(tc: ^TypeChecker, fn: ^ast.FunctionStatement) {
	_, found := lookup_symbol(tc, fn.name.value)
	if found {
		error(tc, fn.name.tok, "Function already declared: %s", fn.name.value)
		return
	}

	for arg in fn.args {
		arg.resolved_type = resolve_type_annotation(tc, arg.declared_type)
	}

	return_type := resolve_type_annotation(tc, fn.declared_return_type)
	if return_type.kind == .Invalid {
		error(
			tc,
			fn.declared_return_type.tok,
			"Invalid type - %s",
			fn.declared_return_type.tok.literal,
		)
		return
	}

	// functions return void if no type is given
	if return_type.kind == .Any {
		return_type.kind = .Void
	}

	// main function is special, check its return type
	// or implicitly set it to Int if no type is given
	if fn.name.value == "main" {
		tc.has_main = true
		// TODO: make sure usermain has the correct signature `fn main(args: []String) -> Int`
		if return_type.kind == .Void {
			return_type.kind = .Int
		} else if return_type.kind != .Int {
			error(
				tc,
				fn.declared_return_type.tok,
				"Expected return type of Int, got %s",
				return_type.kind,
			)
		}
	}

	param_types := make([dynamic]^ast.TypeInfo, tc.allocator)
	for param in fn.args {
		append(&param_types, param.resolved_type)
	}
	fn.resolved_type = make_typeinfo(tc, .Function)
	fn.resolved_type.data = ast.FunctionTypeInfo {
		param_types = param_types,
		return_type = return_type,
	}
	add_symbol(tc, fn.name.value, fn.resolved_type)
}

check_stmt :: proc(
	tc: ^TypeChecker,
	stmt: ast.Statement,
	expected_return_type: ast.TypeKind,
) {
	switch s in stmt {
	case ^ast.ExprStatement:
		check_expr(tc, s.value)

	case ^ast.FunctionStatement:
		check_func_stmt(tc, s)

	case ^ast.ReturnStatement:
		expr_type := check_expr(tc, s.value)
		s.resolved_type = expr_type
		if s.resolved_type.kind != expected_return_type {
			error(
				tc,
				s.tok,
				"Expected return type %s, got %s",
				expected_return_type,
				s.resolved_type.kind,
			)
		}

	case ^ast.AssignStatement:
		check_assign_stmt(tc, s)

	case ^ast.IfStatement:
		check_if_stmt(tc, s, expected_return_type)

	case ^ast.LoopStatement:
		check_loop_stmt(tc, s, expected_return_type)

	case ^ast.BreakStatement:
		// TODO: Not sure if I need to do anything with break statements here
		// error(tc, s.tok, "TODO: break statement not typechecked")
		return
	case ^ast.StructStatement:
		check_struct_stmt(tc, s)

	case ^ast.Module, ^ast.FunctionArg, ^ast.BlockStatement, ^ast.StructField:
		logger.error("Unreachable statement: %+v", stmt)
	}
}

check_struct_stmt :: proc(tc: ^TypeChecker, stmt: ^ast.StructStatement) {
	// registering first, TODO: eventually move this to top-level
	register_struct_symbol(tc, stmt)

	data, ok := stmt.resolved_type.data.(ast.StructTypeInfo)
	if !ok {
		return
	}
	for field in stmt.fields {
		field.resolved_type = resolve_type_annotation(tc, field.type)
		append(&data.fields, field)
	}
}

check_loop_stmt :: proc(
	tc: ^TypeChecker,
	loop: ^ast.LoopStatement,
	expected_return_type: ast.TypeKind,
) {
	switch loop.kind {
	case .Infinite:
		symbols_push_scope(tc)
		defer symbols_pop_scope(tc)
		for stmt in loop.block.stmts {
			check_stmt(tc, stmt, expected_return_type)
		}

	case .When:
		when_typeinfo := check_expr(tc, loop.wehn)
		if when_typeinfo.kind != .Bool {
			error(
				tc,
				ast.get_token_from_expr(loop.wehn),
				"loop when expects a condition that resolves to a boolean, got %s",
				when_typeinfo.kind,
			)
		}
		symbols_push_scope(tc)
		defer symbols_pop_scope(tc)
		for stmt in loop.block.stmts {
			check_stmt(tc, stmt, expected_return_type)
		}

	case .Iterator:
		if loop.item == nil {
			error(
				tc,
				loop.tok,
				"item variable is required when looping over an array",
			)
			return
		}

		symbols_push_scope(tc)
		defer symbols_pop_scope(tc)

		item_ident, ok := loop.item.(^ast.Identifier)
		if !ok {
			error(
				tc,
				ast.get_token_from_expr(loop.item),
				"Expected identifier",
			)
			return
		}
		_, found := lookup_symbol(tc, item_ident.value)
		if found {
			error(
				tc,
				item_ident.tok,
				"Symbol already declared: %s",
				item_ident.value,
			)
			item_ident.resolved_type = make_typeinfo(tc, .Invalid)
			return
		}

		items_typeinfo := check_expr(tc, loop.items)
		#partial switch items_typeinfo.kind {
		case .Array:
			arr_typeinfo := items_typeinfo.data.(ast.ArrayTypeInfo)
			item_ident.resolved_type = make_typeinfo(
				tc,
				arr_typeinfo.elements_type.kind,
			)
			add_symbol(tc, item_ident.value, item_ident.resolved_type)
		case .Slice:
			slice_typeinfo := items_typeinfo.data.(ast.SliceTypeInfo)
			item_ident.resolved_type = make_typeinfo(
				tc,
				slice_typeinfo.elements_type.kind,
			)
			add_symbol(tc, item_ident.value, item_ident.resolved_type)
		case:
			error(
				tc,
				ast.get_token_from_expr(loop.items),
				"Expected array, got %s",
				items_typeinfo.kind,
			)
			return
		}


		// idx is optional
		if loop.idx != nil {
			idx_ident, ok := loop.idx.(^ast.Identifier)
			if !ok {
				error(
					tc,
					ast.get_token_from_expr(loop.idx),
					"Expected idx to be an identifier",
				)
				return
			}
			_, found := lookup_symbol(tc, idx_ident.value)
			if found {
				error(
					tc,
					idx_ident.tok,
					"Symbol already declared: %s",
					idx_ident.value,
				)
				idx_ident.resolved_type = make_typeinfo(tc, .Invalid)
				return
			}
			idx_ident.resolved_type = make_typeinfo(tc, .Int)
			add_symbol(tc, idx_ident.value, idx_ident.resolved_type)
		}

		for stmt in loop.block.stmts {
			check_stmt(tc, stmt, expected_return_type)
		}
	}
}

check_if_stmt :: proc(
	tc: ^TypeChecker,
	s: ^ast.IfStatement,
	expected_return_type: ast.TypeKind,
) {
	cond_typeinfo := check_expr(tc, s.condition)
	if cond_typeinfo.kind != .Bool {
		error(
			tc,
			ast.get_token_from_expr(s.condition),
			"if expression should resolve to a boolean, got %s",
			cond_typeinfo.kind,
		)
		return
	}

	symbols_push_scope(tc)
	for stmt in s.consequence.stmts {
		check_stmt(tc, stmt, expected_return_type)
	}
	symbols_pop_scope(tc)

	if s.alternative != nil {
		symbols_push_scope(tc)
		for stmt in s.alternative.stmts {
			check_stmt(tc, stmt, expected_return_type)
		}
		symbols_pop_scope(tc)
	}
	return
}

check_assign_stmt :: proc(tc: ^TypeChecker, s: ^ast.AssignStatement) {
	_, found := lookup_symbol(tc, s.name.value)
	if found {
		error(tc, s.tok, "Symbol already declared: %s", s.name.value)
		s.resolved_type = make_typeinfo(tc, .Invalid)
		return
	}

	typeinfo := resolve_type_annotation(tc, s.declared_type)
	if typeinfo.kind == .Invalid {
		error(
			tc,
			s.declared_type.tok,
			"Invalid type - %s",
			s.declared_type.tok.type,
		)
		s.resolved_type = make_typeinfo(tc, .Invalid)
		return
	}
	if typeinfo.kind == .Slice {
		error(
			tc,
			s.declared_type.tok,
			"Cannot assign to a slice - to allocate a new array provide the size eg: [5]Int",
		)
		s.resolved_type = make_typeinfo(tc, .Invalid)
		return
	}
	s.resolved_type = typeinfo

	if s.value != nil {
		expr_type := check_expr(tc, s.value)
		if typeinfo.kind == .Any {
			s.resolved_type = expr_type
		} else if typeinfo.kind != expr_type.kind {
			error(
				tc,
				ast.get_token_from_expr(s.value),
				"Type mismatch - declared type: %s, expression type: %s",
				typeinfo.kind,
				expr_type.kind,
			)
			s.resolved_type = make_typeinfo(tc, .Invalid)
			return
		} else if typeinfo.kind == .Array {
			ti_ti := typeinfo.data.(ast.ArrayTypeInfo)
			expr_ti := expr_type.data.(ast.ArrayTypeInfo)
			if ti_ti.elements_type.kind != expr_ti.elements_type.kind {
				error(
					tc,
					ast.get_token_from_expr(s.value),
					"Type mismatch - array elements declared type %s, got %s",
					ti_ti.elements_type.kind,
					expr_ti.elements_type.kind,
				)
				s.resolved_type = make_typeinfo(tc, .Invalid)
				return
			}
			if ti_ti.size < expr_ti.size {
				error(
					tc,
					ast.get_token_from_expr(s.value),
					"Array size mismatch: Declared type expected %d, got %d",
					ti_ti.size,
					expr_ti.size,
				)
				s.resolved_type = make_typeinfo(tc, .Invalid)
				return
			}
			arr_value := s.value.(^ast.Array)
			arr_value.resolved_type = s.resolved_type
		}
	}

	add_symbol(tc, s.name.value, s.resolved_type)
}

check_func_stmt :: proc(tc: ^TypeChecker, fn: ^ast.FunctionStatement) {
	// Since we already checked the function signature, all we need to do is check the body
	symbols_push_scope(tc)
	defer symbols_pop_scope(tc)

	for arg in fn.args {
		add_symbol(tc, arg.ident.value, arg.resolved_type)
	}

	if fn.body == nil {
		error(tc, fn.tok, "Missing function body")
		return
	}

	typeinfo, ok := fn.resolved_type.data.(ast.FunctionTypeInfo)
	if !ok {
		error(
			tc,
			fn.tok,
			"expected functiontypeinfo, got %s",
			fn.resolved_type.kind,
		)
		return
	}

	expected_return_type := typeinfo.return_type.kind
	for stmt in fn.body.stmts {
		if _, ok := stmt.(^ast.FunctionStatement); ok {
			error(tc, fn.body.tok, "nested functions not allowed")
		}
		check_stmt(tc, stmt, expected_return_type)
	}
	return
}

check_body_stmt :: proc(
	tc: ^TypeChecker,
	body: ^ast.BlockStatement,
	declared_return_type: ast.TypeKind,
) {
}

check_expr :: proc(tc: ^TypeChecker, expr: ast.Expr) -> ^ast.TypeInfo {
	switch e in expr {
	case ^ast.Boolean:
		e.resolved_type = make_typeinfo(tc, .Bool)
		return e.resolved_type
	case ^ast.IntLiteral:
		e.resolved_type = make_typeinfo(tc, .Int)
		return e.resolved_type
	case ^ast.StringLiteral:
		e.resolved_type = make_typeinfo(tc, .String)
		return e.resolved_type
	case ^ast.CallExpr:
		return check_call_expr(tc, e)
	case ^ast.InfixExpr:
		return check_infix_expr(tc, e)
	case ^ast.PrefixExpr:
		rhs_type := check_expr(tc, e.right)
		if rhs_type.kind == .Invalid {
			return rhs_type
		}
		e.resolved_type = rhs_type
		return e.resolved_type

	case ^ast.Identifier:
		symbol_type, found := lookup_symbol(tc, e.value)
		if !found {
			error(tc, e.tok, "Identifier not found: %s", e.value)
			e.resolved_type = make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
		e.resolved_type = symbol_type
		e.resolved_type.reassignable = true
		return e.resolved_type
	case ^ast.Array:
		return check_array_expr(tc, e)
	case ^ast.IndexExpr:
		return check_index_expr(tc, e)
	case ^ast.ReassignExpr:
		return check_reassign_expr(tc, e)
	case ^ast.PointerExpr:
		return check_pointer_expr(tc, e)
	case ^ast.DerefExpr:
		return check_deref_expr(tc, e)
	}
	logger.error("Unreachable - checking expr: %+v", expr)
	return nil
}

check_deref_expr :: proc(
	tc: ^TypeChecker,
	expr: ^ast.DerefExpr,
) -> ^ast.TypeInfo {
	rhs := check_expr(tc, expr.operand)
	if rhs.kind != .Pointer {
		tok := ast.get_token_from_expr(expr.operand)
		error(tc, tok, "Cannot dereference a non-pointer")
		return nil
	}

	ptr_typeinfo := rhs.data.(ast.PointerTypeInfo)
	expr.resolved_type = ptr_typeinfo.base_type
	return expr.resolved_type
}

check_pointer_expr :: proc(
	tc: ^TypeChecker,
	expr: ^ast.PointerExpr,
) -> ^ast.TypeInfo {
	if !is_addressable(expr.operand) {
		tok := ast.get_token_from_expr(expr.operand)
		error(tc, tok, "expression is not addressable")
		return nil
	}
	rhs := check_expr(tc, expr.operand)
	if rhs.kind == .Invalid {
		return nil
	}
	typeinfo := make_typeinfo(tc, .Pointer, reassignable = true)
	typeinfo.data = ast.PointerTypeInfo{rhs}
	expr.resolved_type = typeinfo
	return expr.resolved_type
}

is_addressable :: proc(expr: ast.Expr) -> bool {
	#partial switch e in expr {
	case ^ast.Identifier, ^ast.IndexExpr, ^ast.DerefExpr:
		return true
	case:
		return false
	}
}

check_reassign_expr :: proc(
	tc: ^TypeChecker,
	expr: ^ast.ReassignExpr,
) -> ^ast.TypeInfo {
	lhs := check_expr(tc, expr.target)
	if !lhs.reassignable {
		error(
			tc,
			ast.get_token_from_expr(expr.target),
			"Cannot reassign to immutable expression",
		)
		expr.resolved_type = make_typeinfo(tc, .Invalid)
		return expr.resolved_type
	}
	rhs := check_expr(tc, expr.value)
	if lhs.kind != rhs.kind {
		error(
			tc,
			ast.get_token_from_expr(expr.target),
			"Cannot reassign value of type %s to type %s",
			rhs.kind,
			lhs.kind,
		)
		expr.resolved_type = make_typeinfo(tc, .Invalid)
		return expr.resolved_type
	}
	expr.resolved_type = lhs
	return nil
}

check_index_expr :: proc(
	tc: ^TypeChecker,
	expr: ^ast.IndexExpr,
) -> ^ast.TypeInfo {
	ident_typeinfo := check_expr(tc, expr.left)
	if ident_typeinfo.kind != .Array {
		error(
			tc,
			ast.get_token_from_expr(expr.left),
			"Type error - expected Array, got %s",
			ident_typeinfo.kind,
		)
		expr.resolved_type = make_typeinfo(tc, .Invalid)
		return expr.resolved_type
	}

	idx_typeinfo := check_expr(tc, expr.index)
	if idx_typeinfo.kind != .Int {
		error(
			tc,
			ast.get_token_from_expr(expr.index),
			"Type error - expected Int, got %s",
			idx_typeinfo.kind,
		)
		expr.resolved_type = make_typeinfo(tc, .Invalid)
		return expr.resolved_type
	}

	ident_arr_typeinfo := ident_typeinfo.data.(ast.ArrayTypeInfo)
	expr.resolved_type = make_typeinfo(
		tc,
		ident_arr_typeinfo.elements_type.kind,
		true,
	)

	return expr.resolved_type
}

check_array_expr :: proc(tc: ^TypeChecker, e: ^ast.Array) -> ^ast.TypeInfo {
	e.resolved_type = make_typeinfo(tc, .Array)
	if len(e.elements) <= 0 {
		e.resolved_type.data = ast.ArrayTypeInfo {
			elements_type = make_typeinfo(tc, .Any),
			size          = 0,
		}
		return e.resolved_type
	}
	first_typeinfo := check_expr(tc, e.elements[0])
	array_typeinfo := ast.ArrayTypeInfo {
		elements_type = first_typeinfo,
		size          = len(e.elements),
	}

	for el in e.elements[1:] {
		cur := check_expr(tc, el)
		if cur.kind != array_typeinfo.elements_type.kind {
			error(
				tc,
				ast.get_token_from_expr(el),
				"Type mismatch - can only have one element type in an array: expected %d, got %d",
				array_typeinfo.elements_type.kind,
				cur.kind,
			)
			e.resolved_type = make_typeinfo(tc, .Invalid)
			return e.resolved_type
		}
	}

	e.resolved_type.data = array_typeinfo
	return e.resolved_type
}

check_infix_expr :: proc(
	tc: ^TypeChecker,
	e: ^ast.InfixExpr,
) -> ^ast.TypeInfo {
	lhs_type := check_expr(tc, e.left)
	rhs_type := check_expr(tc, e.right)
	if lhs_type.kind == .Invalid || rhs_type.kind == .Invalid {
		error(
			tc,
			e.tok,
			"infix expression contains invalid left: %s, right: %s",
			lhs_type.kind,
			rhs_type.kind,
		)
		e.resolved_type = make_typeinfo(tc, .Invalid)
		return e.resolved_type
	}
	if lhs_type.kind != rhs_type.kind {
		error(
			tc,
			e.tok,
			"type mismatch - expected %s, got %s",
			lhs_type.kind,
			rhs_type.kind,
		)
		e.resolved_type = make_typeinfo(tc, .Invalid)
		return e.resolved_type
	}
	#partial switch e.tok.type {
	case .EQ, .NOT_EQ, .LT, .GT, .LTE, .GTE:
		e.resolved_type = make_typeinfo(tc, .Bool)
	case:
		e.resolved_type = lhs_type
	}
	return e.resolved_type

}

check_call_expr :: proc(tc: ^TypeChecker, e: ^ast.CallExpr) -> ^ast.TypeInfo {
	typeinfo, found := lookup_symbol(tc, e.func.value)
	if !found {
		// TODO: add better builtin support
		if e.func.value == "printf" {
			// TODO: do i need to support variadics now?? (builtins)

			// TODO: partially duplicating the logic that happens lower down because i don't have good builtin support :(
			for arg, i in e.args {
				arg_typeinfo := check_expr(tc, arg)
				if arg_typeinfo.kind == .Invalid {
					error(
						tc,
						ast.get_token_from_expr(arg),
						"invalid function parameter",
					)
					return make_typeinfo(tc, .Invalid)
				}
			}

			return_type := make_typeinfo(tc, .Int)
			f_typeinfo := make_typeinfo(tc, .Function)
			f_typeinfo.data = ast.FunctionTypeInfo {
				return_type = return_type,
			}
			e.resolved_type = f_typeinfo
			return e.resolved_type
		}

		error(tc, e.tok, "Identifier not found: %s", e.func.value)
		return make_typeinfo(tc, .Invalid)
	}

	if typeinfo.kind != .Function {
		error(tc, e.tok, "Identifier is not a function, got %s", typeinfo.kind)
		return make_typeinfo(tc, .Invalid)
	}

	f_typeinfo := typeinfo.data.(ast.FunctionTypeInfo)

	if len(f_typeinfo.param_types) != len(e.args) {
		error(
			tc,
			e.tok,
			"function arity does not match. Signature expects %d parameters, got %d",
			len(f_typeinfo.param_types),
			len(e.args),
		)
		return make_typeinfo(tc, .Invalid)
	}

	for arg, i in e.args {
		arg_typeinfo := check_expr(tc, arg)
		if arg_typeinfo.kind == .Invalid {
			error(
				tc,
				ast.get_token_from_expr(arg),
				"invalid function parameter",
			)
			return make_typeinfo(tc, .Invalid)
		}
		f_param := f_typeinfo.param_types[i]

		if !is_valid_func_arg(f_param, arg_typeinfo) {
			error(
				tc,
				ast.get_token_from_expr(arg),
				"parameter type mismatch: expected %s, got %s",
				f_param.kind,
				arg_typeinfo.kind,
			)
			return make_typeinfo(tc, .Invalid)
		}
	}
	e.resolved_type = f_typeinfo.return_type
	return e.resolved_type
}

is_valid_func_arg :: proc(
	expected: ^ast.TypeInfo,
	actual: ^ast.TypeInfo,
) -> bool {
	if expected.kind == .Slice {
		return actual.kind == .Slice || actual.kind == .Array
	} else {
		return expected.kind == actual.kind
	}
}

error :: proc(tc: ^TypeChecker, tok: token.Token, ft: string, args: ..any) {
	// TODO: I feel like this shouldn't use the temporary allocator
	msg := fmt.tprintf(ft, ..args)
	err := logger.CompilerError {
		msg  = msg,
		line = tok.line,
		col  = tok.col,
	}
	append(&tc.errors, err)
}

add_symbol :: proc(tc: ^TypeChecker, name: string, typeinfo: ^ast.TypeInfo) {
	if len(tc.symbols) <= 0 {
		logger.error("can't add symbol to an empty stack")
		return
	}
	name_copy := strings.clone(name, tc.allocator)
	tc.symbols[len(tc.symbols) - 1][name_copy] = typeinfo
}

lookup_symbol :: proc(
	tc: ^TypeChecker,
	name: string,
) -> (
	^ast.TypeInfo,
	bool,
) {
	for i := len(tc.symbols) - 1; i >= 0; i -= 1 {
		scope := tc.symbols[i]
		if val, ok := scope[name]; ok {
			return val, true
		}
	}
	return nil, false
}

symbols_pop_scope :: proc(tc: ^TypeChecker) {
	if len(tc.symbols) <= 0 {
		logger.error("attemping to pop scope from empty symbols table")
		return
	}
	popped := pop(&tc.symbols)
	delete(popped)
}

symbols_push_scope :: proc(tc: ^TypeChecker) {
	scope := make(map[string]^ast.TypeInfo, tc.allocator)
	append(&tc.symbols, scope)
}

resolve_pointer_type_annotation :: proc(
	tc: ^TypeChecker,
	annotation: ^ast.TypeAnnotation,
) -> ^ast.TypeInfo {
	ptr_ta := annotation.data.(ast.PointerTypeAnnotation)

	base_type := resolve_type_annotation(tc, ptr_ta.base_type)
	typeinfo := make_typeinfo(tc, .Pointer, reassignable = true)
	typeinfo.data = ast.PointerTypeInfo{base_type}
	return typeinfo
}

resolve_array_type_annotation :: proc(
	tc: ^TypeChecker,
	annotation: ^ast.TypeAnnotation,
) -> ^ast.TypeInfo {
	arr_annotation := annotation.data.(ast.ArrayTypeAnnotation)
	size_typeinfo := check_expr(tc, arr_annotation.size_expr)
	if size_typeinfo.kind != .Int {
		error(
			tc,
			ast.get_token_from_expr(arr_annotation.size_expr),
			"Array size must be an Int",
		)
		return make_typeinfo(tc, .Invalid)
	}

	//
	// TODO: support anything that evaluates to an int
	//
	size := arr_annotation.size_expr.(^ast.IntLiteral).value
	elements_type := resolve_type_annotation(tc, arr_annotation.elements_type)

	arr_typeinfo := make_typeinfo(tc, .Array)
	arr_typeinfo.data = ast.ArrayTypeInfo {
		elements_type = elements_type,
		size          = size,
	}
	return arr_typeinfo
}

resolve_slice_type_annotation :: proc(
	tc: ^TypeChecker,
	annotation: ^ast.TypeAnnotation,
) -> ^ast.TypeInfo {
	slice_annotation := annotation.data.(ast.SliceTypeAnnotation)

	elements_type := resolve_type_annotation(
		tc,
		slice_annotation.elements_type,
	)

	slice_typeinfo := make_typeinfo(tc, .Slice)
	slice_typeinfo.data = ast.SliceTypeInfo {
		elements_type = elements_type,
	}
	return slice_typeinfo
}

resolve_type_annotation :: proc(
	tc: ^TypeChecker,
	annotation: ^ast.TypeAnnotation,
) -> ^ast.TypeInfo {
	if annotation == nil {
		return make_typeinfo(tc, .Any)
	}

	#partial switch annotation.tok.type {
	case .TYPE_INT:
		return make_typeinfo(tc, .Int, reassignable = true)
	case .TYPE_STRING:
		return make_typeinfo(tc, .String, reassignable = true)
	case .TYPE_BOOL:
		return make_typeinfo(tc, .Bool, reassignable = true)
	case .TYPE_VOID:
		return make_typeinfo(tc, .Void)
	case .STAR:
		return resolve_pointer_type_annotation(tc, annotation)
	case .IDENT:
		return resolve_struct_type_annotation(tc, annotation)
	case .L_BRACKET:
		#partial switch d in annotation.data {
		case ast.ArrayTypeAnnotation:
			return resolve_array_type_annotation(tc, annotation)
		case ast.SliceTypeAnnotation:
			return resolve_slice_type_annotation(tc, annotation)
		}
	}
	logger.error("Unreachable type annotation: %s", annotation.tok.type)
	return make_typeinfo(tc, .Invalid)
}

resolve_struct_type_annotation :: proc(
	tc: ^TypeChecker,
	annotation: ^ast.TypeAnnotation,
) -> ^ast.TypeInfo {
	ta_data := annotation.data.(ast.StructTypeAnnotation)
	typeinfo, ok := lookup_symbol(tc, ta_data.name.value)
	if !ok {
		error(
			tc,
			annotation.tok,
			"Struct \"%s\" not found",
			ta_data.name.value,
		)
		return nil
	}
	if typeinfo.kind != .Struct {
		error(
			tc,
			annotation.tok,
			"Expecting struct, but got %s",
			typeinfo.kind,
		)
		return nil
	}
	return typeinfo
}

make_typeinfo :: proc(
	tc: ^TypeChecker,
	kind: ast.TypeKind,
	reassignable := false,
) -> ^ast.TypeInfo {
	typeinfo := new(ast.TypeInfo, tc.allocator)
	typeinfo.kind = kind
	typeinfo.reassignable = reassignable
	return typeinfo
}
