package logger

import "core:fmt"

LogLevel :: enum {
	INFO,
	ERROR,
}

log :: proc(level: LogLevel, msg: string, args: ..any) {
	fmt.printf("[%s] ", level)
	fmt.printfln(msg, ..args)
}

info :: proc(msg: string, args: ..any) {
	log(.INFO, msg, ..args)
}

error :: proc(msg: string, args: ..any) {
	log(.ERROR, msg, ..args)
}

CompilerError :: struct {
	msg:  string,
	line: int,
	col:  int,
}

compiler_error :: proc(filename: string, err: CompilerError) {
	fmt.printfln("%s:%d:%d: %s", filename, err.line, err.col, err.msg)
}
