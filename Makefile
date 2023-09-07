UNAME_S = $(shell uname -s)
CFLAGS   = \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration \
	-Iinclude \
	-I.

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

all:: bin/$(BIN) bin/$(BIN2)

tmp/%.lex.c: %.l tmp/%.tab.c tmp
	$(LEX) -o $@ $<

tmp/%.lex.c: %.l tmp
	$(LEX) -o $@ $<

tmp/%.tab.c: %.y tmp
	$(YACC) -d $< -o $@

tmp:
	mkdir -p $@

tmp/%.o: src/%.c tmp
	$(CC) -c $< -o $@ $(CFLAGS)

bin/$(BIN): tmp/$(BIN).lex.c tmp/$(BIN).tab.c $(OBJ)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

bin/$(BIN2): tmp/$(BIN2).lex.c $(OBJ)
	mkdir -p ./bin
	$(CC) -ll -o $@ $^ $(CFLAGS) $(LDFLAGS)

data-test: data-test.c tmp/mylang-data.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean::
	rm -rf $(BIN) $(BIN).yy.c $(BIN).tab.c $(BIN).lex.c $(BIN).tab.h  tmp

test: clean all
	echo "\"hello \\\"bob\\\"\"" | ./bin/$(BIN)
	echo
	echo "(+ 1 'x' 2 \"hello\" 'qwert)" | ./bin/$(BIN)

test2: clean all
	./$(BIN2)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o






