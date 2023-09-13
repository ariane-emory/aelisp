UNAME_S      = $(shell uname -s)

YACC_LEX_CFLAGS = \
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
BIN2     = data_test

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
	GDB = lldb
endif

################################################################################
# Targets
################################################################################

all: bin/ae bin/data_test bin/test/ae_obj_test

obj/%.o: src/%.c obj
	$(CC) -o $@ $< $(LDFLAGS) $(STRICTER_CFLAGS) -c

################################################################################
# Binaries
################################################################################

bin/test/%: bin/test
	$(CC) -o $@ $(patsubst bin/test/%, test/%.c, $@) $(LDFLAGS) $(STRICTER_CFLAGS)

bin/ae: tmp/ae.lex.c tmp/ae.tab.c $(OBJ)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS) $(YACC_LEX_CFLAGS)

bin/data_test: data_test.c obj/ae_obj.o
	$(CC) -o $@ $^ $(LDFLAGS) $(STRICTER_CFLAGS) -Wno-unused-variable

################################################################################
# Utility targets
################################################################################

tests: clean all
	./bin/ae
	./bin/data_test
	./bin/test/ae_obj_test

debug: clean all
	$(GDB) ./bin/ae

################################################################################
# Lexer/parser
################################################################################

tmp/%.lex.c: %.lex tmp/%.tab.c tmp
	$(LEX) -o $@ $<

tmp/%.lex.c: %.lex tmp
	$(LEX) -o $@ $<

tmp/%.tab.c: %.yacc tmp
	$(YACC) -d $< -o $@

################################################################################
# Directories
################################################################################

tmp:
	mkdir -p $@

obj:
	mkdir -p $@

bin:
	mkdir -p $@

bin/test:
	mkdir -p $@

################################################################################
# Clean
################################################################################

clean:
	rm -rf bin obj tmp

