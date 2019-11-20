%include "src/parser.inc"

global write

extern fildes
extern parse
extern tokenid
extern tokenval
extern vlqencode
extern vlqlen
extern written

section .bss
	buffer resb 1024
	onnotes resb 256

section .data
	buflen dd 0
	totlen dd 0
	base dd 70
	channel dd 0
	volume dd 127
	delta dd 64
	nextdelta dd 0
	stopmode dd 0

section .text
write:
	mov eax, [tokenid]
	jmp [eax * 4 + .table]

	.set_base:
		cmp dword [tokenval], 127
		jg parse
		mov eax, [tokenval]
		mov [base], eax
		jmp parse

	.write_instrument:
		cmp dword [tokenval], 127
		jg parse
		mov byte [buffer], 0
		mov eax, 0xC0
		or eax, [channel]
		mov byte [buffer + 1], al
		mov eax, [tokenval]
		mov byte [buffer + 2], al
		mov dword [buflen], 3
		jmp .writeall

	.set_channel:
		cmp dword [tokenval], 15
		jg parse
		mov eax, [tokenval]
		mov [channel], eax
		jmp parse

	.write_tempo:
		mov byte [buffer], 0
		mov byte [buffer + 1], 0xFF
		mov byte [buffer + 2], 0x51
		mov byte [buffer + 3], 0x03
		mov ebx, [tokenval]
		mov eax, 60000000
		mov edx, 0
		div ebx
		shl eax, 8
		bswap eax
		mov dword [buffer + 4], eax

		add dword [buflen], 7
		jmp .writeall

	.write_volume:
		mov ebx, [tokenval]
		cmp ebx, 127
		jg parse
		mov [volume], ebx
		mov byte [buffer], 0
		mov eax, 0xB0
		or eax, [channel]
		mov byte [buffer + 1], al
		mov byte [buffer + 2], 0x07
		mov byte [buffer + 3], bl
		mov dword [buflen], 4
		jmp .writeall

	.set_delta:
		mov eax, [tokenval]
		mov [delta], eax
		jmp parse

	.write_noteon:
		cmp dword [nextdelta], 0
		je .nodelete
		call stopall

		.nodelete:
		push dword 0x90
		mov ecx, [tokenval]
		add ecx, [base]
		push ecx
		call writenote
		pop ecx
		add esp, 4

		mov eax, [delta]
		mov [nextdelta], eax

		mov eax, 1
		mov ebx, ecx
		mov ecx, [channel]
		shl eax, cl
		or [onnotes + ebx * 2], ax

		jmp .writeall

	.write_noteoff:
		mov eax, [tokenval]
		add eax, [base]
		push eax
		call stopnote
		add esp, 4
		jmp parse

	.set_continue:
		mov eax, [delta]
		add dword [nextdelta], eax
		jmp parse

	.set_zerodelta:
		mov dword [nextdelta], 0
		jmp parse

	.set_incdelta:
		shl dword [nextdelta], 1
		jmp parse

	.set_decdelta:
		shr dword [nextdelta], 1
		jmp parse

	.write_incvolume:
		add dword [volume], 5
		mov eax, [volume]
		mov [tokenval], eax
		cmp eax, 123
		jl .write_volume

	.write_decvolume:
		sub dword [volume], 5
		mov eax, [volume]
		mov [tokenval], eax
		cmp eax, 4
		jg .write_volume

	.write_start:
		mov dword [buffer], 0x6468544D
		mov dword [buffer + 4], 0x06000000
		mov word [buffer + 8], 0x0000
		mov word [buffer + 10], 0x0100
		mov word [buffer + 12], 0x4000
		mov dword [buffer + 14], 0x6B72544D
		mov dword [buffer + 18], 0x00FFFF00
		mov dword [buflen], 22
		jmp .writeall

	.write_end:
		mov dword [stopmode], 1
		call stopall

		.write_size:
		mov eax, 19
		mov ebx, [fildes]
		mov ecx, 18
		mov edx, 0
		int 0x80

		cmp eax, 0
		jl .nosize

		mov eax, 0x04
		mov ebx, [fildes]
		mov ecx, [totlen]
		sub ecx, 18
		bswap ecx
		mov [buffer], ecx
		mov ecx, buffer
		mov edx, 4
		int 0x80

		mov eax, 19
		mov ebx, [fildes]
		mov ecx, 0
		mov edx, 2
		int 0x80

		.nosize:
		mov byte [buffer], 0
		mov byte [buffer + 1], 0xFF
		mov byte [buffer + 2], 0x2F
		mov byte [buffer + 3], 0
		mov dword [buflen], 4
		jmp .writeall

	.writeall:
		mov eax, 0x4
		mov ebx, [fildes]
		mov ecx, buffer
		mov edx, [buflen]
		int 0x80

		mov eax, [buflen]
		add [totlen], eax
		mov dword [buflen], 0

		;mov eax, 118
		;mov ebx, 0x01
		;int 0x80

		cmp dword [tokenid], TOKEN_END
		je written
		jmp parse

	.set_stopmode:
		not dword [stopmode]
		jmp parse

	.table:
		dd .set_base
		dd .write_instrument
		dd .set_channel
		dd .write_tempo
		dd .write_volume
		dd .set_delta
		dd .write_noteon
		dd .write_noteoff
		dd .set_continue
		dd .set_zerodelta
		dd .set_incdelta
		dd .set_decdelta
		dd .write_incvolume
		dd .write_decvolume
		dd .write_start
		dd .write_end
		dd .set_stopmode

writedelta:
	push dword [nextdelta]
	call vlqencode
	add esp, 4

	mov ebx, [buflen]
	mov dword [buffer + ebx], eax

	push eax
	call vlqlen
	add esp, 4

	ret

writenote:
	call writedelta
	mov dword [nextdelta], 0
	lea ebx, [buffer + eax]
	add ebx, [buflen]

	mov ecx, [esp + 8]
	or ecx, [channel]

	mov byte [ebx], cl
	mov ecx, [esp + 4]
	mov byte [ebx + 1], cl
	mov byte [ebx + 2], 127
	add [buflen], eax
	add dword [buflen], 3

	ret

stopnote:
	push dword 0x80
	push dword [esp + 8]
	call writenote
	add esp, 8
	mov eax, 1
	mov ecx, [channel]
	mov edx, [esp + 4]
	shl eax, cl
	not eax
	and [onnotes + edx], eax
	ret

stopall:
	push dword [channel]
	mov edx, 128

	.delloop:
		dec edx
		cmp edx, 0
		jl .exit

		mov ax, [onnotes + edx * 2]

		cmp dword [stopmode], 0
		mov ecx, 16
		jne .chanloop

		mov ebx, 1
		mov ecx, [channel]
		shl ebx, cl
		test eax, ebx
		jz .delloop

		push edx
		call stopnote
		pop edx

		jmp .delloop

		.chanloop:
			test ecx, ecx
			jz .delloop

			dec ecx
			mov ebx, 1
			shl ebx, cl
			test eax, ebx
			jz .chanloop

			mov [channel], ecx
			push ecx
			push edx
			call stopnote
			pop edx
			pop ecx

			jmp .chanloop

	.exit:
	pop dword [channel]

	ret
