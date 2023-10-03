UNAME_S = $(shell uname -s)

COMMON_CFLAGS = \
	-ggdb \
	-Iinclude \
	-I . \
	-Wno-misleading-indentation \
	-DAE_CALLSTACK_IS_PROPER \
	-DAE_DEADLY_MARGIN \
	-DAE_OBJ_DEBUG \
	-DAE_OBJ_DEBUG_TRACK_ORIGIN \
	-DAE_DUMP_POOL_AFTER \
	-DAE_LEXICAL_SCOPING \

UNUSED_CFLAGS = \
	-DAE_CORE_ENVS \
	-DAE_LOG_EVAL \
	-DAE_DUMP_POOL_BEFORE \
	-DAE_LOG_CORE \
	-DAE_LOG_ALLOC \
	-DAE_LOG_CLONE \
	-DAE_LOG_ENV \
	-DAE_LOG_FREE_LIST \
	-DAE_LOG_INIT \
	-DAE_LOG_LEX \
	-DAE_LOG_METADATA \
	-DAE_LOG_MOVE \
	-DAE_LOG_PARSE \
	-DAE_LOG_SYM \
	-DAE_NO_SINGLE_SYM_PARAMS \
	-DAE_OBJ_POOL_SIZE=4096 \
  -DAE_LOG_CONS \
  -DAE_LOG_PUSH \

TEST_CFLAGS = \
	-Wno-unused-value

BIN_CFLAGS = \

YACC_LEX_CFLAGS = \
	-Wno-implicit-int \
	-Wno-implicit-function-declaration \
	-Wno-address-of-packed-member

STRICTER_CFLAGS = \
	-I3p/acutest/include/ \
	-Werror \
	-Wall \
	-Wextra \
	-Wno-format \
	-Wno-comment \
	-Wno-address-of-packed-member \
	-Wno-unused-variable \
	-Wno-unused-but-set-variable \
	-DACUTEST_SPACES=80 \
	-DACUTEST_IGNORE_EXIT

ifeq ($(UNAME_S),Darwin)
	CXX = g++-13
  CC  = gcc-13
	GDB = lldb
endif

GDB       = gdb
OBJDUMP   = objdump
LEX       = flex
YACC      = bison
YACC      = /opt/homebrew/opt/bison/bin/bison

SRCS      = $(shell find src  -name "*.c")
TEST_SRCS = $(shell find test -name "*.c")
OBJS      = $(patsubst src/%.c, obj/%.o, $(SRCS))
TEST_BINS = $(patsubst test/%.c, bin/test/%, $(TEST_SRCS))

################################################################################
# Targets
################################################################################

all: bin/ae $(TEST_BINS)

obj/%.o: src/%.c obj
	$(CC) -o $@ $< $(LDFLAGS) $(COMMON_CFLAGS) $(STRICTER_CFLAGS) -c

################################################################################
# Executables
################################################################################

bin/test/%: bin/test
	$(CC) -o $@ $(patsubst bin/test/%, test/%.c, $@) $(OBJS) $(LDFLAGS) $(COMMON_CFLAGS) $(STRICTER_CFLAGS) $(TEST_CFLAGS)

bin/ae: tmp/ae.l.c tmp/ae.tab.c $(OBJS)
	mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS) $(BIN_CFLAGS)

################################################################################
# Lexer/parser
################################################################################

tmp/%.lex.c: grammar/%.l tmp/%.tab.c tmp
	$(LEX) -o $@ $<

tmp/%.l.c: grammar/%.l tmp
	$(LEX) -o $@ $<

tmp/%.tab.c: grammar/%.y tmp
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
	./bin/ae
	$(foreach bin, $(TEST_BINS), $(bin))

debug: clean all
	$(GDB) ./bin/ae

################################################################################
# Clean
################################################################################

clean:
	rm -rf bin obj tmp

