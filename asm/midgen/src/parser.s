%include "src/parser.inc"

global parse
global tokenid
global tokenval

extern parseint
extern getchar
extern write

section .data
	tokenid dd 0
	tokenval dd 0
	state dd STATE_START
	numstr db '0000000000000000'
	numlen dd 0
	errorstring db 'Errore di sintassi.', 10, 0

section .text
	parse:
		call getchar
		mov ebx, [state]
		shr ebx, 8
		jmp [ebx * 4 + .table]

		.null:
			cmp eax, '0'
			jl .nonumber
			cmp eax, '9'
			jg .nonumber
			jmp .number

			.nonumber:
			test eax, eax
			jz .end
			cmp eax, 0xFFFFFFFF
			jz .end
			cmp eax, '('
			je .char_bl
			cmp eax, '!'
			je .char_neg
			cmp eax, ':'
			je .char_colon
			cmp eax, '+'
			je .char_plus
			cmp eax, '-'
			je .char_minus
			cmp eax, '^'
			je .char_caret
			cmp eax, '/'
			je .char_slash
			cmp eax, '.'
			je .char_dot
			cmp eax, ';'
			je .char_semicolon
			cmp eax, '@'
			je .char_at
			cmp eax, 'n'
			mov ebx, TOKEN_BASE
			je .setting
			cmp eax, 'i'
			mov ebx, TOKEN_INSTRUMENT
			je .setting
			cmp eax, 'c'
			mov ebx, TOKEN_CHANNEL
			je .setting
			cmp eax, 't'
			mov ebx, TOKEN_TEMPO
			je .setting
			cmp eax, 'v'
			mov ebx, TOKEN_VOLUME
			je .setting
			cmp eax, 'd'
			mov ebx, TOKEN_DELTA
			je .setting
			jmp parse

		.setting_open:
			cmp eax, '<'
			jne .syntaxerror
			mov eax, [state]
			mov dword [state], STATE_SETTING_NUMBER
			or [state], al
			jmp parse

		.setting_number:
			cmp eax, '>'
			je .setting_end
			jmp .addnumber

		.noteoff_open:
			cmp eax, '('
			jne .syntaxerror
			mov dword [state], STATE_LONGNOTE_NUMBER
			or dword [state], STATE_ATTRIB_OFF
			jmp parse

		.longnote_number:
			cmp eax, ')'
			je .longnote_end
			jmp .addnumber

		.start:
			mov dword [tokenid], TOKEN_START
			mov dword [state], STATE_NULL
			jmp write

		.end:
			mov dword [state], STATE_END
			mov dword [tokenid], TOKEN_END
			jmp write

		.number:
			mov dword [tokenid], TOKEN_NOTEON
			sub eax, '0'
			mov [tokenval], eax
			jmp write

		.char_bl:
			mov dword [state], STATE_LONGNOTE_NUMBER
			jmp parse

		.char_neg:
			mov dword [state], STATE_NOTEOFF_OPEN
			jmp parse

		.char_colon:
			mov dword [tokenid], TOKEN_ZERODELTA
			jmp write

		.char_plus:
			mov dword [tokenid], TOKEN_INCDELTA
			jmp write

		.char_minus:
			mov dword [tokenid], TOKEN_DECDELTA
			jmp write

		.char_caret:
			mov dword [tokenid], TOKEN_INCVOLUME
			jmp write

		.char_slash:
			mov dword [tokenid], TOKEN_DECVOLUME
			jmp write

		.char_dot:
			mov dword [tokenid], TOKEN_CONTINUE
			jmp write

		.char_at:
			mov dword [tokenid], TOKEN_STOPMODE
			jmp write

		.char_semicolon:
			mov dword [tokenid], TOKEN_END
			mov dword [state], STATE_END
			jmp write

		.setting:
			mov dword [state], STATE_SETTING_OPEN
			or [state], bl
			jmp parse

		.addnumber:
			mov ebx, [numlen]

			cmp ebx, 15
			jg .syntaxerror

			mov [numstr + ebx], eax
			inc dword [numlen]
			jmp parse

		.setting_end:
			mov eax, [state]
			and eax, 0xFF
			mov [tokenid], eax

			mov eax, [numlen]
			mov dword [numstr + eax], 0
			push dword numstr
			call parseint
			add esp, 4

			mov [tokenval], eax
			mov dword [numlen], 0
			mov dword [state], STATE_NULL
			jmp write

		.longnote_end:
			mov eax, [state]
			and eax, 0xFF
			add eax, TOKEN_NOTEON

			mov [tokenid], eax

			mov eax, [numlen]
			mov dword [numstr + eax], 0
			push dword numstr
			call parseint
			add esp, 4

			mov [tokenval], eax
			mov dword [numlen], 0
			mov dword [state], STATE_NULL
			jmp write

		.syntaxerror:
			mov eax, 0x4
			mov ebx, 0x2
			mov ecx, errorstring
			mov edx, 22
			int 0x80
			mov dword [state], STATE_END
			jmp parse

		.table:
			dd .null
			dd .null
			dd .setting_open
			dd .setting_number
			dd .noteoff_open
			dd .longnote_number
			dd .start
			dd .end
