package codegen

import "../fs"
import "../logger"
import os "core:os/os2"
import "core:strings"

compile_qbe :: proc(sb: ^strings.Builder, program_name: string) -> bool {
	qbe_file := fs.create_file_name(program_name, "ssa", context.temp_allocator)
	asm_file := fs.create_file_name(program_name, "s", context.temp_allocator)
	defer free_all(context.temp_allocator)

	err := os.write_entire_file(qbe_file, sb.buf[:])
	if err != nil {
		logger.error("os write file error: %+v", err)
		return false
	}

	// QBE -> ASM
	{
		cmd := []string{"qbe", "-o", asm_file, qbe_file}
		logger.info("Running command: %v", cmd)

		desc := os.Process_Desc {
			command = cmd,
			stdin   = os.stdin,
			stdout  = os.stdout,
			stderr  = os.stderr,
		}
		pid, err := os.process_start(desc)
		if err != nil {
			logger.error("os process_start error: %+v", err)
			return false
		}
		state, pid_err := os.process_wait(pid)
		if pid_err != nil {
			logger.error("os process_wait pid_err: %+v", err)
			return false
		}
		if !state.success {
			return false
		}
	}

	// ASM -> Executable
	{
		cmd := []string{"gcc", "-o", program_name, asm_file}
		logger.info("Running command: %v", cmd)

		desc := os.Process_Desc {
			command = cmd,
			stdin   = os.stdin,
			stdout  = os.stdout,
			stderr  = os.stderr,
		}
		pid, err := os.process_start(desc)
		if err != nil {
			logger.error("os process_start error: %+v", err)
			return false
		}

		state, pid_err := os.process_wait(pid)
		if pid_err != nil {
			logger.error("os process_wait pid_err: %+v", err)
			return false
		}
		if !state.success {
			return false
		}
	}

	return true
}
