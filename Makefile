UNAME_S      = $(shell uname -s)
YCFLAGS       = \
	-Iinclude \
	-I. \
	-Werror \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration \
  -ggdb
#	-Wfatal-errors \

CFLAGS = \
	-Iinclude \
	-I. \
	-Werror \
	-Wall \
	-Wextra \
	-Wshadow \
	-Wpedantic \
	-Wno-format \
	-ggdb
#	-Wfatal-errors \

GDB      = gdb
OBJDUMP  = objdump
LEX      = flex
YACC     = bison
SRC      = $(shell find src -name "*.c")
OBJ      = $(patsubst src/%.c, obj/%.o, $(SRC))
BIN      = ae
BIN2     = mini
BIN3     = data_test

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
	GDB = lldb
endif

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
	$(CC) -o $@ $^ $(YCFLAGS) $(LDFLAGS)

bin/$(BIN2): tmp/$(BIN2).lex.c $(OBJ)
	mkdir -p ./bin
	$(CC) -ll -o $@ $^ $(YCFLAGS) $(LDFLAGS)

bin/$(BIN3): $(BIN3).c obj/ae_obj.o obj/ae_list.o 
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS) -Wno-unused-variable

clean::
	rm -rf bin obj tmp

test: clean all
	echo "(a b) (a b c) (a b c d (e f)) (a b c d \"bang\" 1 1.5 3/4 'x' ?\\y () nil 'a' '\\t')" | ./bin/$(BIN) || :
#	echo "('\\n' '\\t')" | ./bin/$(BIN)
#	echo "(a b c) (a 1.234 2. -.5 b c d e \"hello!\" -128/36 'a' '\\\n' ?\\x) (+ 1 'x' 2 \"hello\"  qwert)" | ./bin/$(BIN)
	./bin/$(BIN3) || :
#	$(GDB) ./bin/$(BIN3)

test2: clean all
	./$(BIN2)

debug: clean all
	$(GDB) ./$(BIN)

dump: clean tmp/main.o $(BIN)
	$(OBJDUMP) -t -d tmp/main.o

