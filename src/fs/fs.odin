package fs

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

read_entire_file :: proc(file_name: string, allocator: mem.Allocator) -> string {
	data, ok := os.read_entire_file_from_filename(file_name, allocator)
	if !ok {
		return ""
	}
	return string(data)
}

extract_base_name :: proc(file_name: string) -> string {
	dot_index := strings.last_index_byte(file_name, '.')
	if dot_index != -1 {
		return file_name[:dot_index]
	}
	return file_name
}

create_file_name :: proc(base_name: string, ext: string, allocator: mem.Allocator) -> string {
	sb: strings.Builder
	strings.builder_init(&sb, allocator)
	defer strings.builder_destroy(&sb)
	fmt.sbprintf(&sb, "%s.%s", base_name, ext)
	return strings.to_string(sb)
}
