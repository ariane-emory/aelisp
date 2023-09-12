UNAME_S      = $(shell uname -s)
CFLAGS       = -Iinclude -I. -Wno-implicit-int -Wno-implicit-function-declaration	-ggdb

EXTRA_CFLAGS = \
	-Iinclude \
	-I. \
	-Werror \
	-Wall \
	-Wextra \
	-Wshadow \
	-Wpedantic \
	-Wno-format \
	-ggdb

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
BIN2     = data_test

all:: bin/$(BIN) bin/$(BIN2)

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
	$(CC) -c $< -o $@ $(EXTRA_CFLAGS) 

bin/$(BIN): tmp/$(BIN).lex.c tmp/$(BIN).tab.c $(OBJ)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

bin/$(BIN2): $(BIN2).c obj/ae_obj.o
	$(CC) -o $@ $^ $(EXTRA_CFLAGS) $(LDFLAGS) -Wno-unused-variable

clean::
	rm -rf bin obj tmp

test: clean all
	./bin/$(BIN)
	./bin/$(BIN2)

test2: clean all
	./$(BIN2)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o

