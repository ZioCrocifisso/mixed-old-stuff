global _start
extern getopt
extern optarg
extern optind
extern optopt

section .data
	opts db ':f:b:sri', 0

	usage:
		db 'Usage: echocol [-f foreground color] [-b background color] [-s | -r] [-i] text', 10
		db '	-f <n>		Foreground color (0-7)', 10
		db '	-b <n>		Background color (0-7)', 10
		db '	-s		Blink (slow)', 10
		db '	-r		Blink (rapid)', 10
		db '	-i		Bold/Intensity', 10, 10
		db 'Example: echocol -f 4 -b 7 -i example text', 10
	usage_len equ $ - usage

	unr db 'Unrecognized option.', 10, 10
	unr_len equ $ - unr

	missing db 'Missing argument.', 10
	missing_len equ $ - missing

	eow db ' '
	eol db 10

	%macro escape 1
		%strlen len %1
		%%start:
		db 0x1B, '[', %1, 'm'
		times (8 - (3 + len)) db 0
	%endmacro

	escape_fcolors:
		escape '30'
		escape '31'
		escape '32'
		escape '33'
		escape '34'
		escape '35'
		escape '36'
		escape '37'

	escape_bcolors:
		escape '40'
		escape '41'
		escape '42'
		escape '43'
		escape '44'
		escape '45'
		escape '46'
		escape '47'

	escape_slow:
		escape '5'
	
	escape_rapid:
		escape '6'

	escape_boldintensity:
		escape '1'

	escape_reset:
		escape '0'

	escape1_len: equ 4
	escape2_len: equ 5

section .text
	%macro print 2
		mov rax, 1
		mov rdi, 1
		mov rsi, %1
		mov rdx, %2
		syscall
	%endmacro

	_start:
		mov rdi, [rsp]
		mov rsi, rsp
		add rsi, 8
		mov rdx, opts
		call getopt
		cmp rax, 'f'
		je .foreground
		cmp rax, 'b'
		je .background
		cmp rax, 's'
		je .slowblink
		cmp rax, 'r'
		je .rapidblink
		cmp rax, 'i'
		je .boldintensity
		cmp rax, '?'
		je .unrecognized
		cmp rax, ':'
		je .missing
		jmp .printtext

		.foreground:
			mov rbx, [optarg]
			mov rcx, 0
			mov cl, [rbx]
			sub cl, '0'
			lea rbx, [escape_fcolors + rcx * 8]
			print rbx, escape2_len
			jmp _start

		.background:
			mov rbx, [optarg]
			mov rcx, 0
			mov cl, [rbx]
			sub cl, '0'
			lea rbx, [escape_bcolors + rcx * 8]
			print rbx, escape2_len
			jmp _start

		.slowblink:
			print escape_slow, escape1_len
			jmp _start

		.rapidblink:
			print escape_rapid, escape1_len
			jmp _start

		.boldintensity:
			print escape_boldintensity, escape1_len
			jmp _start

		.unrecognized:
			print unr, unr_len
			print usage, usage_len
			jmp _start

		.missing:
			print missing, missing_len
			print usage, usage_len
			jmp _start

		.printtext:
			mov ebx, [rsp]
			cmp dword [optind], ebx
			jge .end

			mov rbx, rsp
			add rbx, 8
			mov rax, 0
			mov eax, [optind]
			shl eax, 3
			mov rbx, [rbx + rax]

			mov rax, 0
			mov rdi, rbx

			.searchend:
				scasb
				jnz .searchend

			mov rcx, rdi
			sub rcx, rbx
			print rbx, rcx
			print eow, 1

			inc dword [optind]
			jmp .printtext

		.end:
			print escape_reset, escape1_len
			print eol, 1

			mov rax, 60
			mov rdi, 0
			syscall

			ret
