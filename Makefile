UNAME_S      = $(shell uname -s)
CFLAGS       = \
	-ggdb \
	-Iinclude \
	-I. \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration \
	-DNOISY_INIT

STRICTER_CFLAGS = \
  -ggdb \
	-Iinclude \
	-I. \
	-I3p/acutest/include/ \
	-Werror \
	-Wall \
	-Wextra \
	-Wshadow \
	-Wpedantic \
	-Wno-format \
	-DNOISY_INIT

GDB      = gdb
OBJDUMP  = objdump
LEX      = flex
YACC     = bison
SRC      = $(shell find src -name "*.c")
OBJ      = $(patsubst src/%.c, obj/%.o, $(SRC))
BIN      = ae
BIN2     = data_test

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
	GDB = lldb
endif

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
	$(CC) -o $@ $< $(LDFLAGS) $(STRICTER_CFLAGS) -c

bin/$(BIN): tmp/$(BIN).lex.c tmp/$(BIN).tab.c $(OBJ)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS) $(CFLAGS)

bin/$(BIN2): $(BIN2).c obj/ae_obj.o
	$(CC) -o $@ $^ $(LDFLAGS) $(STRICTER_CFLAGS) -Wno-unused-variable

clean::
	rm -rf bin obj tmp

test: clean all
	./bin/$(BIN) | tee out
	./bin/$(BIN2)

test2: clean all
	./$(BIN2)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o

