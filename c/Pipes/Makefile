CC=gcc
CFLAGS=-g `sdl-config --cflags`
LDFLAGS=`sdl-config --libs`

OBJDIR=obj
SRCDIR=src
BINDIR=bin

BFILES=graphics interface main config pipes
OBJS=$(addprefix $(OBJDIR)/,$(addsuffix .o,$(BFILES)))
SRCS=$(addprefix $(SRCDIR)/,$(addsuffix .c,$(BFILES)))
BIN=pipes

all: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $(BINDIR)/$(BIN)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	-rm -f $(OBJDIR)/*