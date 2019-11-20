global getchar
global headstrp

section .data
	from dd getchar.fromnothing
	lastchar db 0
	db 0, 0, 0
	headstrp dd 0

section .text
getchar:
	jmp dword [from]

	.fromstdin:
		mov eax, 0x03
		mov ebx, 0x0
		mov ecx, lastchar
		mov edx, 1
		int 0x80

		cmp eax, 1
		jne .eof

		mov eax, 0
		mov al, [lastchar]
		ret

		.eof:
		mov eax, 0xFFFFFFFF
		ret

	.fromhead:
		mov eax, 0
		mov ebx, [headstrp]
		mov al, [ebx]

		test eax, eax
		jz .headend
		inc dword [headstrp]
		ret

		.headend:
		mov dword [from], .fromstdin
		jmp .fromstdin

	.fromnothing:
		mov eax, 0xFFFFFFFF
		mov dword [from], .fromhead
		ret
