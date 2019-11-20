extern belf
global _start

section .data
	plus_instr:
		inc qword [r14 + r15]
	plus_instr_len equ $ - plus_instr

	minus_instr:
		dec qword [r14 + r15]
	minus_instr_len equ $ - minus_instr

	abright_instr:
		cmp r15, 0x10000 - 1
		db 0x74, (abright_jmp_target - abright_jmp_next)
		abright_jmp_next:
		inc r15
		abright_jmp_target:
	abright_instr_len equ $ - abright_instr

	ableft_instr:
		cmp r15, 0
		db 0x74, (ableft_jmp_target - ableft_jmp_next)
		ableft_jmp_next:
		dec r15
		ableft_jmp_target:
	ableft_instr_len equ $ - ableft_instr

	sbright_instr:
		db 0xe9
		addr_right: dd 0
	sbright_instr_len: equ $ - sbright_instr

	sbleft_instr:
		cmp byte [r14 + r15], 0
		db 0x0F, 0x84
		addr_left: dd 0
	sbleft_instr_len: equ $ - sbleft_instr

	dot_instr:
		mov rax, qword 1
		mov rdi, qword 1
		lea rsi, [r14 + r15]
		mov rdx, qword 1
		syscall
	dot_instr_len: equ $ - dot_instr

	comma_instr:
		mov rax, qword 0
		mov rdi, qword 0
		lea rsi, [r14 + r15]
		mov rdx, qword 1
		syscall
	comma_instr_len: equ $ - comma_instr

	end_instr:
		mov rax, qword 60
		mov rdi, qword 0
		syscall
		ret
	end_instr_len: equ $ - end_instr

	char db 0

section .text
	%macro brk 1
		mov rax, 12
		lea rdi, %1
		syscall
		mov r14, rax
	%endmacro

	%macro instr 1
		mov rcx, %1_instr_len
		mov rsi, %1_instr
		lea rdi, [r13 + r15]
		rep movsb
		add r15, %1_instr_len
	%endmacro

	%macro icase 1
		.%1:
			instr %1
			jmp .loop
	%endmacro

	_start:

		%define INSTR_MAX 30

		brk [0]
		mov r13, rax
		mov r15, qword 0

		.loop:
			lea rax, [r14 - INSTR_MAX]
			sub rax, r13
			cmp r15, rax
			jl .skipbrk

			mov rax, INSTR_MAX
			brk [r14 + rax * 8]

			.skipbrk:
				mov rax, 0
				mov rdi, 0
				mov rsi, char
				mov rdx, 1
				syscall
				cmp rax, 1
				jl .end

				mov rbx, 0
				mov bl, byte [char]
				mov rbx, [.jmptab + rbx * 8]
				jmp rbx

				icase plus
				icase comma
				icase minus
				icase dot
				icase ableft
				icase abright

				.comment:
					jmp .loop

				.sbleft:
					push r15
					instr sbleft
					jmp .loop

				.sbright:
					pop rbx
					mov rax, rbx
					sub rax, r15
					sub rax, sbright_instr_len
					mov dword [addr_right], eax
					instr sbright

					mov rax, r15
					sub rax, rbx
					sub rax, sbleft_instr_len
					add rbx, r13
					add rbx, (addr_left - sbleft_instr)
					mov dword [rbx], eax

					jmp .loop

		.end:
		instr end
		mov rdi, r13
		mov rsi, r15
		call belf

		brk [r13]

		mov rax, 60
		mov rdi, 1
		syscall
		ret

		.jmptab:
			times 43 dq .comment
			dq .plus
			dq .comma
			dq .minus
			dq .dot
			times 13 dq .comment
			dq .ableft
			dq .comment
			dq .abright
			times 28 dq .comment
			dq .sbleft
			dq .comment
			dq .sbright
			times 162 dq .comment
