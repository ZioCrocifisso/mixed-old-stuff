CC=cc
LD=cc
LIBS=glfw3
CFLAGS=`pkg-config --cflags $(LIBS)`
LDFLAGS=`pkg-config --static --libs $(LIBS)`

OBJS=obj/chunk.o obj/error.o obj/graphics.o obj/world.o obj/main.o

all: obj $(OBJS)
	$(LD) $(OBJS) $(LDFLAGS) -o bin/mcc

obj:
	mkdir obj

obj/%.o: src/%.c
	$(CC) $(CFLAGS) -c $^ -o $@

clean:
	rm obj/*
