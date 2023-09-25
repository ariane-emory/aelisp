%{
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"
#include "ae_env.h"
#include "ae_free_list.h"
#include "ae_write.h"
#include "ae_util.h"

#define YYSTYPE ae_obj_t *

/* #define NL      putchar('\n') */
/* #define BSPC    putchar('\b') */
/* #define SPC     putchar(' ') */
/* #define LPAR    putchar('(') */
/* #define RPAR    putchar(')') */
/* #define LSQR    putchar('[') */
/* #define RSQR    putchar(']') */

#ifdef AE_LOG_PARSE
#  define LOG_PARSE(obj, ...)                                                                      \
  printf(__VA_ARGS__);                                                                             \
  if (obj)                                                                                         \
    PUT((obj));                                                                                    \
  else                                                                                             \
    PR("NULL!");                                                                                   \
  putchar(' ');                                                                                    \
  putchar('\n');                                                                                   \
  fflush(stdout)
#else
#  define LOG_PARSE(obj, ...) ((void)(obj))
#endif

  ae_obj_t * root    = 0;
  
#define free_list_size (1 << 12)

  static char mem[free_list_size] = { 0 };
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }

  int  yywrap() { return 1; }

  void describe(ae_obj_t * this) {
    static unsigned int indent = 0;

    int written = 0;

    while (written++ < indent << 1) SPC;

    written += printf("%018p", this);
    
    while (written++ < 27) SPC;

    written += PUT(this);
    
    while (written++ < 103) SPC;

    ae_put_words(this);

    NL;
    
    if (CONSP(this)) {
      ++indent;
      FOR_EACH(elem, this) {
        describe(elem);
        if (! TAILP(CDR(position))) {
          describe(CDR(position));
        }
      }
      --indent;
    }
  }

  void do_write(ae_obj_t * this) {
    ae_write(this);
    NL;
  }

  extern FILE * yyin;

  //////////////////////////////////////////////////////////////////////////////
  // main
  //////////////////////////////////////////////////////////////////////////////

  main() {
    putchar('\n');

    printf("obj size:     %d.\n",    sizeof(ae_obj_t));
    printf("int size:     %d.\n",    sizeof(int));
    printf("nil at:       %016p.\n", NIL);
    printf("Pool first:   %016p.\n", pool_first);
    printf("Pool last:    %016p.\n", pool_last);
    printf("Pool size:    %016p (%zu bytes).\n",
           sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE,
           sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE);
    printf("Strings size: %016p (%zu bytes).\n\n", free_list_size, free_list_size);

    free_list_add_block(&mem[0], free_list_size);
    
    FILE * fp = fopen("data/sample.txt", "r");
    yyin = fp;
    yyparse();

    printf("root:    ");
    if (root)
      ae_put(root);
    else
      PR("NULL!");
    NL;

    ae_obj_t * program_obj = root;

    printf("program: ");

    if (! program_obj) {
      PR("NULL!");
      return 0;
    }
    else {
      ae_put(program_obj);
    }
    
    NL;

    ae_obj_t * env = NEW_ENV(NIL, NIL, NIL);
    
    pool_print();
    NL;

    puts("Describing items in program.");
    EACH(program_obj, describe);
    puts("Described items in program.");
    NL;

    fputs("Count items in program obj: ", stdout); 
    printf("%d", LENGTH(program_obj));
    NL;
    NL;
    
    puts("Writing items in program obj.");
    if (CONSP(program_obj) && CAR(program_obj))
      EACH(program_obj, do_write);
    puts("Wrote items in program obj.");
    NL;
    puts("Writing interned symbols.");
    ae_write(symbols_list);
    puts("\nWrote interned symbols.");    
  }

    //////////////////////////////////////////////////////////////////////////////
    // End of main
    //////////////////////////////////////////////////////////////////////////////
    
    %}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR INF NILTOK DOT

%start program

%%

program: sexps { root = $$; }

sexps: sexp sexps_tail {
  LOG_PARSE($1, "Consing  ");
  $$ = CONS($1, $2);
  LOG_PARSE($$, "Made     ");
} | {
  $$ = NIL;
};

sexps_tail: sexp sexps_tail {
  LOG_PARSE($1, "Consing  ");
  $$ = CONS($1, $2);
  LOG_PARSE($$, "Made     ");
} | {
  $$ = NIL;
};

sexp: LPAREN sexp_list RPAREN {
  $$ = $2;
} | atom;

sexp_list: sexp sexp_list {
  LOG_PARSE($1, "Consing  ");
  $$ = CONS($1, $2);
  LOG_PARSE($$, "Made     ");
} | {
  $$ = NIL;
};

sexp_list: sexp {
  $$ = $1;
} | sexp DOT sexp {
  $$ = NEW_CONS($1, $3);
};

list: LPAREN sexp_list RPAREN {
  $$ = $2;
};

atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

%%

