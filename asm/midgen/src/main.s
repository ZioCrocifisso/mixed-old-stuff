global _start
global written
global fildes

extern headstrp
extern parse

section .data
	emptystring dd 0
	fildes dd 1

section .text
_start:
	pop ecx
	cmp ecx, 2
	jl .emptyhead

	add esp, 4
	pop eax
	mov [headstrp], eax

	cmp ecx, 3
	jl .noprint

	mov eax, 0x05
	pop ebx
	mov ecx, 1101Q
	mov edx, 644Q
	int 0x80

	cmp eax, 0
	jl .noprint
	mov [fildes], eax
	jmp .noprint

	.emptyhead:
	mov dword [headstrp], emptystring
	jmp .noprint

	.noprint:
	jmp parse
		
	written:
	mov eax, 0x01
	mov ebx, 0x00
	int 0x80
	ret
