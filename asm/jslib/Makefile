lib: jslib.o
	ar rcs libjslib.a jslib.o

test: test.o jslib.o
	gcc test.o jslib.o -o test

test.o: test.c
	gcc -c test.c -o test.o

jslib.o: jslib.asm
	nasm -felf64 jslib.asm -o jslib.o

clean:
	rm test.o jslib.o
