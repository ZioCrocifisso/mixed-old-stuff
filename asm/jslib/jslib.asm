global js_open
global js_update
global js_onpress
global js_onrelease
global js_onmove
global js_close

section .data
	joyactive dq 0				;Joystick in uso

section .bss
	joysticks resb 512			;Alloca per 16 joystick

section .text
	js_open:
		;Trova un joystick non usato
		mov rbx, 0
		mov ax, word [joyactive]
		not ax
		bsf bx, ax
		jz .error

		;Apre il device
		mov rax, 2			;Syscall open()
		;mov rdi, rdi			;Device da aprire
		mov rsi, 2			;Flags (O_RDONLY)
		mov rdx, 0			;Mode (non serve)
		syscall
		cmp rax, -1
		jle .error
		mov r15, rax

		;Indica che il joystick è in uso
		bts [joyactive], bx

		;Imposta il file descriptor
		mov rdi, rbx
		call rdioffset
		mov qword [joysticks + rdi], r15

		;Azzera i puntatori delle funzioni
		mov qword [joysticks + rdi + 8], 0
		mov qword [joysticks + rdi + 16], 0
		mov qword [joysticks + rdi + 24], 0

		;Restituisce l'id del joystick
		mov rax, rbx
		ret

		;Restituisce un errore
		.error:
		mov rax, -1
		ret

	js_update:
		push rbp
		mov rbp, rsp
		sub rsp, 8
		push rdi

		;Legge un evento
		mov rax, 0			;Syscall read()
		call rdioffset
		cmp rdi, -1
		je .end
		mov rbx, rdi
		mov rdi, [joysticks + rbx]	;Carica il file descriptor del device
		lea rsi, [rbp - 8]		;Buffer
		mov rdx, 8			;Dimensione del buffer
		syscall
		cmp rax, -1
		je .end
		pop rdi

		;Imposta i parametri fissi per ogni funzione
		;mov rdi, rdi			;Imposta come primo parametro l'id del joystick
		mov rsi, [rbp - 1]		;Come secondo parametro il tasto/asse dell'evento
		and rsi, 0xFF

		;Controlla il tipo di evento
		mov rax, 0
		mov al, byte [rbp - 2]
		cmp rax, 2
		je .axis
		jg .end				;Evento init, che non è rilevante

		;Chiama la procedura da chiamare quando viene premuto un bottone
		cmp word [rbp - 4], 0		;Controlla se è stato rilasciato
		je .released

		;Tasto premuto
		mov r10, [joysticks + rbx + 8]	;Carica il puntatore a onpress
		jmp .call

		;Tasto rilasciato
		.released:
		mov r10, [joysticks + rbx + 16]	;Carica il puntatore a onrelease
		jmp .call

		;Asse mosso
		.axis:
		mov dx, word [rbp - 4]		;Imposta come terzo parametro lo spostamento
		mov r10, [joysticks + rbx + 24]	;Carica il puntatore a onmove
		jmp .call

		;Controlla se può, e chiama
		.call:
		cmp r10, 0
		je .end
		call r10
		jmp .end

		.end:
		add rsp, 8
		pop rbp
		ret

	js_onpress:
		call rdioffset
		cmp rdi, -1
		je .end
		mov [joysticks + rdi + 8], rsi
		.end:
		ret

	js_onrelease:
		call rdioffset
		cmp rdi, -1
		je .end
		mov [joysticks + rdi + 16], rsi		 
		.end:
		ret

	js_onmove:
		call rdioffset
		cmp rdi, -1
		je .end
		mov [joysticks + rdi + 24], rsi
		.end:
		ret

	js_close:
		btc word [joyactive], di
		call rdioffset
		cmp rdi, -1
		je .end
		mov rdi, [joysticks + rdi]	;Carica il file descriptor del device
		mov rax, 3			;Syscall close()
		syscall
		.end:
		ret

	rdioffset:
		shl rdi, 5
		ret

