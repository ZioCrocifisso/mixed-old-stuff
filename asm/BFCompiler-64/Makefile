bfc: belf.o bfc.o
	ld $^ -o $@

install:
	install -m 755 bfc /usr/bin/bfc

%.o: %.asm
	nasm -felf64 $< -o $@

clean:
	rm belf.o bfc.o bfc
