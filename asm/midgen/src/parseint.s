global parseint

section .text
parseint:
	push dword [esp + 4]
	push dword 0
	call _parseint
	add esp, 8
	ret

_parseint:
	mov edx, [esp + 8]
	mov ecx, 0
	mov cl, [edx]
	test ecx, ecx
	jz .end

	sub ecx, 0x30
	mov ebx, 10
	mov eax, [esp + 4]
	mul ebx
	add eax, ecx

	add dword [esp + 8], 1
	mov dword [esp + 4], eax
	jmp _parseint

	.end:
	mov eax, [esp + 4]
	ret
