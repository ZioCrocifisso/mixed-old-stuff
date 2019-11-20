global belf

section .data
	_start_addr equ 0x08048000

	elf_header:
		e_ident db 0x7f, 'ELF', 2, 1, 1, 3, 0, 0, 0, 0, 0, 0, 0, 16
		e_type dw 2
		e_machine dw 62
		e_version dd 1
		e_entry dq _p2_vaddr
		e_phoff dq (program_header - elf_header)
		e_shoff dq (section_table - elf_header)
		e_flags dd 0
		e_ehsize dw (elf_header_end - elf_header)
		e_phentsize dw (phe2 - phe1)
		e_phnum dw 2
		e_shentsize dw (sh2 - sh1)
		e_shnum dw 4
		e_shstrndx dw 1

	elf_header_end:
	section_table:
		sh0:
			sh0_name dd 0
			sh0_type dd 0
			sh0_flags dq 0
			sh0_addr dq 0
			sh0_offset dq 0
			sh0_size dq 0
			sh0_link dd 0
			sh0_info dd 0
			sh0_addralign dq 0
			sh0_entsize dq 0

		sh1:
			sh1_name dd (string_shstrtab - string_table)
			sh1_type dd 3
			sh1_flags dq 0
			sh1_addr dq 0
			sh1_offset dq (string_table - elf_header)
			sh1_size dq (string_table_end - string_table)
			sh1_link dd 0
			sh1_info dd 0
			sh1_addralign dq 0
			sh1_entsize dq 0

		sh2:
			sh2_name dd (string_data - string_table)
			sh2_type dd 1
			sh2_flags dq 0b011
			sh2_addr dq _p1_vaddr
			sh2_offset dq (data_segment - elf_header)
			sh2_size dq (data_segment_end - data_segment)
			sh2_link dd 0
			sh2_info dd 0
			sh2_addralign dq 8
			sh2_entsize dq 0

		sh3:
			sh3_name dd (string_text - string_table)
			sh3_type dd 1
			sh3_flags dq 0b110
			sh3_addr dq _p2_vaddr
			sh3_offset dq (text_segment - elf_header)
			sh3_size dq (text_segment_end - text_segment)
			sh3_link dd 0
			sh3_info dd 0
			sh3_addralign dq 8
			sh3_entsize dq 0

	program_header:
		phe1:
			p1_type dd 1
			p1_flags dd 0b110
			_p1_offset equ (data_segment - elf_header)
			p1_offset dq _p1_offset
			_p1_vaddr equ (_p1_offset + _start_addr)
			p1_vaddr dq _p1_vaddr
			p1_paddr dq _p1_vaddr
			p1_filesz dq (text_segment - data_segment)
			p1_memsz dq (text_segment - data_segment)
			_p1_align equ 2 << 20
			p1_align dq _p1_align
		phe2:
			p2_type dd 1
			p2_flags dd 0b101
			_p2_offset equ (text_segment - elf_header)
			p2_offset dq _p2_offset
			_p2_vaddr equ (_p2_offset + _start_addr)
			p2_vaddr dq _p2_vaddr
			p2_paddr dq _p2_vaddr
			p2_filesz dq (eof - text_segment)
			p2_memsz dq (eof - text_segment)
			_p2_align equ 2 << 20
			p2_align dq _p2_align

	string_table:
		string_shstrtab db '.shstrtab', 0
		string_data db '.data', 0
		string_text db '.text', 0
	string_table_end:

	data_segment:
		times 0x10000 db 0
	data_segment_end:

	text_segment:
		mov r13, dword _p2_vaddr
		mov r14, dword _p1_vaddr
		mov r15, 0
	text_segment_end:

	eof:

section .text
	belf:
		add qword [p2_filesz], rsi
		add qword [p2_memsz], rsi
		add qword [sh3_size], rsi

		push rdi
		push rsi

		mov rax, 1
		mov rdi, 1
		mov rsi, elf_header
		mov rdx, (eof - elf_header)
		syscall

		mov rax, 1
		mov rdi, 1
		pop rdx
		pop rsi
		syscall

		ret
