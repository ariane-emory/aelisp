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
LEX      = flex
YACC     = bison
SRC      = $(shell find src -name "*.c")
OBJ      = $(patsubst src/%.c, obj/%.o, $(SRC))
BIN      = ae
BIN2     = mini
BIN3     = data_test

all:: bin/$(BIN) bin/$(BIN2) bin/$(BIN3)

tmp/%.lex.c: %.lex tmp/%.tab.c tmp
	$(LEX) -o $@ $<

tmp/%.lex.c: %.lex tmp
	$(LEX) -o $@ $<

tmp/%.tab.c: %.yacc tmp
	$(YACC) -d $< -o $@

tmp:
	mkdir -p $@

obj:
	mkdir -p $@

obj/%.o: src/%.c obj
	$(CC) -c $< -o $@ $(CFLAGS)

bin/$(BIN): tmp/$(BIN).lex.c tmp/$(BIN).tab.c $(OBJ)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

bin/$(BIN2): tmp/$(BIN2).lex.c $(OBJ)
	mkdir -p ./bin
	$(CC) -ll -o $@ $^ $(CFLAGS) $(LDFLAGS)

bin/$(BIN3): $(BIN3).c obj/ae_data.o obj/ae_list.o obj/ae_type.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean::
	rm -rf bin obj tmp

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

