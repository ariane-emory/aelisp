UNAME_S = $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
endif

GDB      = gdb
OBJDUMP  = objdump
SRC      = $(shell find . -name "*.c")
OBJ      = $(patsubst %.c, tmp/%.o, $(SRC))
BIN      = mylang
CFLAGS   = \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration

all:: $(BIN) 

lex.yy.c:
	lex $(BIN).l

y.tab.c:
	yacc -d $(BIN).y

tmp/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) -c $< -o $@ $(CFLAGS)

$(BIN): lex.yy.c y.tab.c
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean::
	rm -rf $(BIN) lex.yy.c y.tab.c y.tab.h tmp

test: clean all
	./$(BIN)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o
