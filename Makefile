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

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
	GDB = lldb
endif

GDB       = gdb
OBJDUMP   = objdump
LEX       = flex
YACC      = bison

SRCS      = $(shell find src  -name "*.c")
TEST_SRCS = $(shell find test -name "*.c")
OBJS      = $(patsubst src/%.c, obj/%.o, $(SRCS))
TEST_BINS = $(patsubst test/%.c, bin/test/%, $(TEST_SRCS))

################################################################################
# Targets
################################################################################

all: bin/ae bin/data_test $(TEST_BINS)

obj/%.o: src/%.c obj
	$(CC) -o $@ $< $(LDFLAGS) $(STRICTER_CFLAGS) -c

################################################################################
# Binaries
################################################################################

bin/test/%: bin/test
	$(CC) -o $@ $(patsubst bin/test/%, test/%.c, $@) $(patsubst bin/test/%_test, obj/%.o, $@) $(LDFLAGS) $(STRICTER_CFLAGS)

bin/ae: tmp/ae.lex.c tmp/ae.tab.c $(OBJS)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS) $(YACC_LEX_CFLAGS)

bin/data_test: $(OBJS)
	$(CC) -o $@ $(patsubst bin/%, %.c, $@) $^ $(LDFLAGS) $(STRICTER_CFLAGS) -Wno-unused-variable

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
# Utility targets
################################################################################

tests: clean all
	#./bin/ae
	./bin/data_test
	$(foreach bin, $(TEST_BINS), $(bin))

debug: clean all
	$(GDB) ./bin/ae

################################################################################
# Clean
################################################################################

clean:
	rm -rf bin obj tmp

