%{
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"
#include "ae_free_list.h"

#define YYSTYPE ae_obj_t *

#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')


#ifdef AE_LOG_PARSE
#  define LOG_PARSE(obj, ...)                                                                      \
  printf(__VA_ARGS__);                                                                             \
  PUT((obj));                                                                                      \
  putchar(' ');                                                                                    \
  putchar('\n');                                                                                   \
  fflush(stdout)
#else
#  define LOG_PARSE(obj, ...) ((void)(obj))
#endif

  ae_obj_t * root    = 0;
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }

  int  yywrap() { return 1; }

  void describe(ae_obj_t * this) {
    static unsigned int indent = 0;
    
    for (int ct = 0; ct < indent << 1; ct++)
      SPC;
    
    PUT(this);
    NL;
    
    if (CONSP(this)) {
      ++indent;
      EACH(this, describe);
      --indent;
    }
  }

  void do_write(ae_obj_t * this) {
    ae_obj_write(this);
    NL;
  }

  extern FILE * yyin;

  //////////////////////////////////////////////////////////////////////////////
  // main
  //////////////////////////////////////////////////////////////////////////////
  
  main() {
    putchar('\n');

    printf("nil at:     %p.\n", NIL);
    printf("Pool first: %p.\n", pool_first);
    printf("Pool last:  %p.\n", pool_last);
    printf("Pool size:  %p (%zu bytes).\n\n",
           sizeof(ae_obj_t) * ((pool_last - pool_first) + 1),
           sizeof(ae_obj_t) * ((pool_last - pool_first) + 1));

    const size_t free_list_size = 1 << 11;
    void * mem = malloc(free_list_size);
    free_list_add_block(mem, free_list_size);

    if (false) 
      for (int ix = 0; ix < 32000; ix++) {
        char * str = (char*)free_list_malloc(1 << 7);
        if (! str) {
          free_list_reset();
          memset(mem, 0, free_list_size);
          free_list_add_block(mem, free_list_size);
        
          continue;
        } 
        sprintf(str, "string pool size: %d bytes.\n", free_list_size);
        puts(str);
      }
    
    FILE * fp = fopen("data/sample.txt", "r");
    yyin = fp;
    yyparse();

    printf("root:    ");
    ae_obj_put(root);
    NL;

    ae_obj_t * program_obj = root;

    printf("program: ");
    ae_obj_put(program_obj);
    NL;

    pool_print();
    NL;

    // return 0;
    
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
    ae_obj_write(symbols_list);
    puts("\nWrote interned symbols.");    
  }

    //////////////////////////////////////////////////////////////////////////////
    // End of main
    //////////////////////////////////////////////////////////////////////////////
    
    %}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR LIST INF NILTOK
%start program

%%

program: sexps { root = $$; }

atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

list:
LPAREN sexps RPAREN { $$ = $2; }
| LIST { INIT($$, AE_CONS); }

sexp: list | atom

sexps:
sexps sexp {
  if (NILP($$)) {
    LOG_PARSE($2, "Beginning with ");
    $$ = CONS($2, $$);
    LOG_PARSE($$, "Made           ");
  }
  else {
    LOG_PARSE($2, "Appending      ");
    PUSH($$, $2);    
    LOG_PARSE($$, "Made           ");
  }
}
| {
  $$ = NIL;
};
   
%%
