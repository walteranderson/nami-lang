package main

import "core:fmt"

@(export)
lib_print :: proc(msg: cstring) {
	fmt.println(string(msg))
}
