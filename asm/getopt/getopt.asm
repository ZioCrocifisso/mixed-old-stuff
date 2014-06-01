global getopt

global optarg
global opterr
global optind
global optopt

section .data
	optarg dq 0
	opterr dd 0
	optind dd 1
	optopt dd 0

section .text
	getopt:
		mov rbx, 0
		mov ebx, [optind]

		cmp ebx, edi
		jge .end

		mov rbx, [rsi + rbx * 8]
		cmp rbx, 0
		je .end

		cmp byte [rbx], '-'
		jne .end

		inc rbx
		mov rax, qword 0
		mov al, [rbx]

		cmp al, 0
		je .end

		cmp al, '-'
		je .last

		mov r15, rdx
		.loop:
			mov cl, [rdx]
			cmp cl, 0
			je .unknown

			cmp al, cl
			je .found

			inc rdx
			jmp .loop

		.found:
		cmp byte [rdx + 1], ':'
		jne .next

		inc rbx
		cmp byte [rbx], 0
		je .argvarg

		mov [optarg], rbx
		jmp .next

		.argvarg:
		inc dword [optind]
		cmp dword [optind], edi
		jge .missing

		mov rbx, 0
		mov ebx, dword [optind]
		mov rbx, [rsi + rbx * 8]

		mov qword [optarg], rbx
		jmp .next

		.missing:
		mov [optopt], al

		cmp byte [r15], ':'
		jne .unknown

		mov rax, ':'
		jmp .next

		.unknown:
		mov rax, '?'
		jmp .next

		.last:
		mov rax, -1

		.next:
		inc dword [optind]
		ret

		.end:
		mov rax, -1
		ret
