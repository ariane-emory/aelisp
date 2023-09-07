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
BIN2     = mini
YACC     = bison
LEX      = flex

all:: $(BIN)

%.lex.c: %.l
	$(LEX) -o mylang.lex.c $^

%.tab.c: %.y
	$(YACC) -d $^

tmp/%.o: src/%.c
	mkdir -p $(dir $@)
	$(CC) -c $< -o $@ $(CFLAGS)

$(BIN): $(BIN).lex.c $(BIN).tab.c $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

$(BIN2): $(BIN2).lex.c $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean::
	rm -rf $(BIN) $(BIN)g.yy.c $(BIN)g.tab.c tmp

test: clean all
	echo "\"hello \\\"bob\\\"\"" | ./$(BIN)
	echo
	echo "(+ 1 2 \"hello\" 'qwert)" | ./$(BIN)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o






