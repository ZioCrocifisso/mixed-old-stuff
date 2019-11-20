global vlqencode
global vlqlen

section .text
vlqencode:
	mov eax, 0
	mov ecx, 0
	mov edx, [esp + 4]
	jmp .next

	.next:
		inc ecx

		shl eax, 8
		or al, dl
		or al, 0x80

		shr edx, 7

		test edx, edx
		jnz .next

	mov ebx, 0x7FFFFFFF

	.shiftloop:
		rol ebx, 8
		dec ecx
		test ecx, ecx
		jnz .shiftloop

	and eax, ebx

	ret

vlqlen:
	mov eax, 0
	mov ebx, [esp + 4]

	.loop:
		inc eax

		mov ecx, ebx
		and ecx, 0x80

		shr ebx, 8
		test ecx, ecx
		jnz .loop

	ret
