UNAME_S = $(shell uname -s)
CFLAGS   = \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration \
	-Iinclude

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
endif

GDB      = gdb
OBJDUMP  = objdump
SRC      = $(shell find src -name "*.c")
OBJ      = $(patsubst src/%.c, tmp/%.o, $(SRC))
BIN      = mylang
YACC     = bison

all:: $(BIN) 

lex.yy.c:
	lex $(BIN).l

mylang.tab.c:
	$(YACC) -d $(BIN).y

tmp/%.o: src/%.c
	mkdir -p $(dir $@)
	$(CC) -c $< -o $@ $(CFLAGS)

$(BIN): lex.yy.c mylang.tab.c $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean::
	rm -rf $(BIN) mylang.yy.c mylang.tab.c y.tab.h tmp

test: clean all
	echo "\"hello \\\"bob\\\"\"" | ./$(BIN)
	echo
	echo "(+ 1 2 \"hello\" 'qwert)" | ./$(BIN)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o






