SHELL        = bash
UNAME_S      = $(shell uname -s)
SRCS         = $(shell find src  -name "*.c" -a -not -name "main.c" -a -not -name "repl.c" -a -not -name "test.c")
OBJS         = $(patsubst src/%.c, obj/%.o, $(SRCS)) obj/ae.tab.o obj/ae.l.o
INCLUDE_DIRS = $(foreach dir, $(shell find include -type d) 3p/bestline, -I$(dir) -Itmp) 
POOL_SIZE   := $(shell echo "$$(( 1 << 24))" )
# TEST_SRCS    = $(shell find test -name "*.c")
# TEST_BINS    = $(patsubst test/%.c, bin/%-test, $(TEST_SRCS))

COMMON_CFLAGS = \
	$(INCLUDE_DIRS) \
	-ggdb \
	-O0 \
	-Wno-misleading-indentation \
	-Wno-unused-label \
	-DAE_OBJ_POOL_SIZE=$(POOL_SIZE) \
	-DAE_DEADLY_MARGIN \
  -D_GNU_SOURCE \
	-DCOUNT_ALLOCATIONS \
	-DAE_LOG_ENV \

UNUSED_CFLAGS = \
	-DAE_LOG_FREE_LIST \
	-DAE_DUMP_SYMS \
	-DAE_LOG_ALLOC \
	-DAE_LOG_CLONE \
	-DAE_LOG_CONS \
	-DAE_LOG_INIT \
	-DAE_LOG_KVP_SET_GET \
	-DAE_LOG_LEX \
	-DAE_LOG_METADATA \
	-DAE_LOG_MOVE \
	-DAE_LOG_PARSE \
	-DAE_LOG_PUSH \
	-DAE_LOG_SYM \
	-DAE_DUMP_POOL_AFTER \
	-DAE_DUMP_POOL_BEFORE \
	-DAE_NO_SINGLE_SYM_PARAMS \
	-DAE_PAINT_EARLY_OBJECTS \
	-DAE_PREFACE \
	-DAE_PREFER_ALIST \

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
YACC      = /opt/homebrew/opt/bison/bin/bison

ifeq ($(wildcard /opt/homebrew/opt/bison/bin/bison),)
    YACC = bison
else
    YACC = /opt/homebrew/opt/bison/bin/bison
endif

################################################################################
# Targets
################################################################################

all: bin/ae bin/repl bin/test

obj/bestline.o:
	$(CC) -o $@ 3p/bestline/bestline.c $(COMMON_CFLAGS) $(STRICTER_CFLAGS) -c

obj/ae.l.o:  tmp/ae.l.c tmp/ae.tab.h obj
	$(CC) -o $@ $< $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS) -c

obj/ae.tab.o:  tmp/ae.tab.c tmp/ae.tab.h obj
	$(CC) -o $@ $< $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS) -c

-include $(OBJS:.o=.d)

obj/%.o: src/%.c obj obj/core obj/test
	$(CC) -o $@ $< $(LDFLAGS) $(COMMON_CFLAGS) $(STRICTER_CFLAGS) -c -MMD -MP 

################################################################################
# Executables
################################################################################

bin/test: test.c $(OBJS) 
	mkdir -p bin
	$(CC) -o $@ $^ $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS) $(STRICTER_CFLAGS)

bin/ae: ae.c $(OBJS)
	mkdir -p bin
	$(CC) -o $@ $^ $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS)

bin/repl: repl.c $(OBJS) obj/bestline.o 
	mkdir -p bin
	$(CC) -o $@ $^ $(LDFLAGS) $(COMMON_CFLAGS) $(YACC_LEX_CFLAGS)

################################################################################
# Lexer/parser
################################################################################

tmp/%.l.c: grammar/%.l tmp
	$(LEX) -o $@ $<

tmp/%.tab.c: grammar/%.y tmp
	$(YACC) -d $< -o $@

tmp/%.tab.h: grammar/%.y tmp
	$(YACC) -d $< -o tmp/ae.tab.c

################################################################################
# Directories
################################################################################

tmp:
	mkdir -p $@

obj:
	mkdir -p $@

obj/core:
	mkdir -p $@

obj/test:
	mkdir -p $@

bin:
	mkdir -p $@

################################################################################
# Utility targets
################################################################################

tests: clean all
	./bin/ae

debug: clean all
	$(GDB) ./bin/ae

################################################################################
# Clean
################################################################################

clean:
	rm -rf bin obj tmp

