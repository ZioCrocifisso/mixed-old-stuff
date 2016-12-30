CC=gcc
CFLAGS=-g
LDFLAGS=-lz -lm
OBJS=connection.o entities.o events.o main.o packets.o player.o world.o

all: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o skelter
    
.c.o: $(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o
