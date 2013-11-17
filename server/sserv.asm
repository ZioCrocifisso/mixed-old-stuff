section .data
	server_addr:					;Indirizzo del server
		dw 2					;P/AF_INET (Famiglia dell'indirizzo)
		server_port dw 0x901f			;Porta (in big endian)
		server_in_addr dd 0			;Indirizzo IP (0.0.0.0, ovvero qualunque)
		dq 0					;Padding (sin_zero)

	addr_len equ 16					;Dimensione della struttura dell'indirizzo

	sleep_timespec:					;Tempo di attesa
		dq 0					;Secondi
		dq 500000000				;Nanosecondi

	bufmax equ 128
	buffer: times bufmax db 0			;Buffer che conterrà i dati ricevuti

	se_str db `HTTP/1.1 500 Internal Server Error\r\n\r\nInternal Server Error.\r\n`
	se_len equ $ - se_str

	br_str db `HTTP/1.1 400 Bad Request\r\n\r\nBad Request.\r\n`
	br_len equ $ - se_len

	nf_str db `HTTP/1.1 404 Not Found\r\n\r\nNot Found.\r\n`
	nf_len equ $ - nf_str

	ok1_str db `HTTP/1.1 200 OK\r\nContent-length: `
	ok1_len equ $ - ok1_str
	ok2_str db `\r\nConnection: close\r\n\r\n`
	ok2_len equ $ - ok2_str

	true dd 1
	index_str db 'index.html', 0
	index_len equ $ - index_str

	numbuf times 20 db ' '				;Buffer in cui contenere i numeri


section .text
_start:
	;Creazione del socket
	mov rax, 41					;Codice della syscall socket()
	mov rdi, 2					;P/AF_INET (Famiglia del protocollo)
	mov rsi, 1					;SOCK_STREAM (Tipo di protocollo)
	mov rdx, 0					;Protocollo (non necessario, viene scelto automaticamente)
	syscall						;Invoca la syscall
	cmp rax, -1					;Controlla se ci sono stati errori
	jle error
	mov r12, rax					;Copia il risultato (id del socket) nel registro 12

	;Imposta l'opzione di riutilizzo dell'indirizzo
	mov rax, 54					;Syscall setsockopt()
	mov rdi, r12
	mov rsi, 1					;SOL_SOCKET (livello di setsockopt)
	mov rdx, 2					;SO_REUSEADDR (riutilizzare l'indirizzo)
	mov r10, true					;Puntatore al valore dell'opzione
	mov r8, 4					;Dimensione del valore
	syscall

	;Impostazione dell'indirizzo di ascolto
	mov rax, 49					;Syscall bind()
	mov rdi, r12					;File descriptor del socket
	mov rsi, server_addr				;Puntatore a indirizzo del server
	mov rdx, addr_len				;Dimensione dell'indirizzo
	syscall
	cmp rax, 0
	jne error

	;Ascolto per le connessioni
	mov rax, 50					;Syscall listen()
	mov rdi, r12
	mov rsi, 10					;Backlog (limite di connessioni in coda)
	syscall
	cmp rax, -1
	jle error

	.clientloop:
		;Connessione ai client
		mov rax, 43				;Syscall accept()
		mov rdi, r12
		mov rsi, 0				;Puntatore all'indirizzo del client (0 per ignorarlo)
		mov rdx, 0				;Puntatore alla dimensione dell'indirizzo
		syscall
		cmp rax, -1
		jle error
		mov r13, rax				;File descriptor del socket per il client

		;Ricezione dei dati
		mov rax, 45				;Syscall recvfrom()
		mov rdi, r13
		mov rsi, buffer				;Puntatore al buffer che conterrà i dati ricevuti
		mov rdx, (bufmax - 1)			;Dimensione del buffer
		mov r10, 0
		mov r8, 0
		mov r9, 0
		syscall
		cmp rax, -1
		jle .servererror
		mov r15, rax

		mov rcx, 0
		.searchloop:				;Cerca il carattere '/' indicante l'inizio del percorso
			cmp [buffer + rcx], byte '/'
			je .sendfile
			inc rcx
			cmp rcx, r15			;Se finisce il buffer e non trova il percorso
			jge .badrequest			;Spedisce un errore 400 (bad request)
			jmp .searchloop

		.servererror:
			mov r14, se_str			;Sceglie la stringa per l'errore 500
			mov r15, se_len			;Imposta la sua lunghezza
			call send
			jmp .close

		.badrequest:
			mov r14, br_str			;Sceglie la stringa per l'errore 400
			mov r15, br_len 		;Dimensione
			call send
			jmp .close

		.notfound:
			mov r14, nf_str			;Sceglie la stringa per l'errore 404
			mov r15, nf_len 		;Dimensione
			call send
			jmp .close

		.sendfile:
			inc rcx
			lea rbx, [buffer + rcx]
			sub r15, rcx
			mov rcx, 0
			.endloop:			;Cerca lo spazio che indica la fine del percorso
				cmp [rbx + rcx], byte ' '
				je .verifypath
				inc rcx
				cmp rcx, r15
				jge .badrequest
				jmp .endloop

			.verifypath:
				mov [rbx + rcx], byte 0
				cmp rcx, 0
				jne .openfile
				mov rbx, index_str
				mov rcx, index_len

			.openfile:
				mov rax, 2		;Syscall open()
				mov rdi, rbx		;Percorso
				mov rsi, 0		;O_RDONLY (sola lettura)
				mov rdx, 0		;(in questo caso non serve)
				syscall
				cmp rax, -1
				jle .notfound
				mov rbx, rax		;Salva l'fd in un registro appartenente al chiamante

			;Invia la prima parte degli header
			mov r14, ok1_str
			mov r15, ok1_len
			call send

			;Ottiene la dimensione del file
			mov rdi, rbx
			call filesize

			;La converte in una stringa
			mov rdi, rax			;Numero da inviare
			mov rsi, numbuf			;Buffer in cui verrà contenuto il numero
			call itoa

			;La invia
			mov r14, numbuf			;Stringa contentente il numero
			mov r15, rax			;Sua dimensione
			call send

			;Invia il resto degli header
			mov r14, ok2_str
			mov r15, ok2_len
			call send

			;Legge e invia bufmax byte alla volta
			.sendloop:
				mov rax, 0		;Syscall read()
				mov rdi, rbx		;File descriptor del file aperto
				mov rsi, buffer		;Puntatore al buffer
				mov rdx, bufmax		;Dimensione massima del buffer
				syscall
				cmp rax, 0
				jle .close

				mov r14, buffer
				mov r15, rax
				call send

				jmp .sendloop
				

		.close:
		;Chiude il file
		mov rax, 3				;Syscall close()
		mov rdi, rbx
		syscall

		;Comunica che il socket verrà chiuso
		mov rax, 48				;Syscall shutdown()
		mov rdi, r13
		mov rsi, 0
		syscall

		;Attende per un po'
		mov rax, 35				;Syscall nanosleep()
		mov rdi, sleep_timespec
		mov rsi, 0
		syscall

		;Chiude il socket del client
		mov rax, 3				;Syscall close()
		mov rdi, r13
		syscall

		jmp .clientloop

send:
	;Invio dei dati
	mov rax, 44					;Syscall sendto()
	mov rdi, r13					;File descriptor del client
	mov rsi, r14					;Puntatore ai dati da inviare
	mov rdx, r15					;Dimensione dei dati da inviare
	mov r10, 0x8000					;Flags
	mov r8, 0					;Puntatore all'indirizzo (ignorato)
	mov r9, 0					;Dimensione dell'indirizzo (ignorata)
	syscall
	ret

error:
	neg rax
	mov rdi, rax					;Codice dell'errore
	mov rax, 60					;Syscall exit()
	syscall
	ret

filesize:
	;Si sposta verso la fine e ottiene la posizione
	mov rax, 8					;Syscall lseek()
	;mov rdi, rdi					;Il primo argomento è già il file
	mov rsi, 0					;Offset 0
	mov rdx, 2					;SEEK_END (dalla fine del file)
	syscall
	mov r10, rax					;Salva l'indirizzo

	;Torna all'inizio del file
	mov rax, 8
	;mov rdi, rdi
	mov rsi, 0
	mov rdx, 0					;SEEK_SET
	syscall

	mov rax, r10					;Risultato
	ret

itoa:
	mov rax, rdi
	mov rcx, 1					;Numero di cifre
	mov r15, 10

	.divloop:
		xor rdx, rdx
		div r15
		add dl, '0'				;Trasforma il resto della divisione in un carattere ASCII
		mov rdi, 20
		sub rdi, rcx				;Calcola la posizione dalla fine
		mov byte [rdi + rsi], dl		;Copia il resto della divisione nel buffer

		cmp rcx, 20
		jge .return
		inc rcx
		jmp .divloop

	.return:
		mov rax, 20				;TODO: spostare
		ret
