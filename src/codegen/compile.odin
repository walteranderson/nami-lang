package codegen

import "../fs"
import "../logger"
import os "core:os/os2"
import "core:strings"

compile_qbe :: proc(qbe: ^Qbe, program_name: string) -> (err: os.Error) {
	qbe_file := fs.create_file_name(program_name, "ssa", context.temp_allocator)
	asm_file := fs.create_file_name(program_name, "s", context.temp_allocator)
	defer free_all(context.temp_allocator)

	os.write_entire_file(qbe_file, qbe.sb.buf[:]) or_return

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
		pid := os.process_start(desc) or_return
		_, pid_err := os.process_wait(pid)
		if pid_err != nil {
			return pid_err
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
		pid := os.process_start(desc) or_return
		_, pid_err := os.process_wait(pid)
		if pid_err != nil {
			return pid_err
		}
	}

	return nil
}
