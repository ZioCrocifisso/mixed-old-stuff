OBJS=obj/parseint.o obj/getchar.o obj/vlq.o obj/parser.o obj/writer.o obj/main.o

midgen: obj $(OBJS)
	ld $(OBJS) -o midgen

install: midgen
	install -m 755 midgen /usr/bin/midgen

obj:
	mkdir obj

obj/%.o: src/%.s
	nasm -f elf32 $< -o $@

.PHONY: clean
clean:
	rm -rf obj midgen
