section .data
	data dq 0
	inc: dq 0.0000000000005
	mul: dq 0xFFFFFF
	fname db '/dev/fb0', 0

section .text
_start:
	fldz

	.fbloop:
		mov rax, 2
		mov rdi, fname
		mov rsi, 2
		mov rdx, 0
		syscall
		mov r12, rax

		.writeloop:
			fadd qword [inc]
			fld st0
			fcos
			fld qword [mul]
			fmulp st1, st0
			fstp qword [data]
			mov rax, 1
			mov rdi, r12
			mov rsi, data
			mov rdx, 8
			syscall
			cmp eax, 0
			jge .writeloop

		fld qword [inc]
		fld qword [inc]
		fadd
		fsin
		fstp qword [inc]
		fstp st0
		fldz
		mov rax, 3
		mov rdi, r12
		syscall
		jmp .fbloop
