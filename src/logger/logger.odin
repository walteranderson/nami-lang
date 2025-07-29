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
