%{
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"

#define YYSTYPE ae_obj_t *

#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')
#define OBJ(x)  ae_obj_put(x)

  ae_obj_t * root    = 0;
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int  yywrap() { return 1; }

  void describe(ae_obj_t * this) {
    static unsigned int indent = 0;
    
    for (int ct = 0; ct < indent << 1; ct++)
      SPC;
    
    OBJ(this);
    NL;
    
    if (CONSP(this) && CAR(this)) {
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

#ifdef POOL_SIZE
  extern ae_obj_t pool[POOL_SIZE];
#endif

  //////////////////////////////////////////////////////////////////////////////
  // main
  //////////////////////////////////////////////////////////////////////////////
  
  main() {
//    symbols_list      = NEW(AE_CONS);
    
    putchar('\n');

    printf("nil at:     %p.\n", NIL);
    printf("Pool first: %p.\n", pool_first);
    printf("Pool last:  %p.\n", pool_last);
    printf("Pool size:  %p (%zu bytes).\n\n",
           sizeof(ae_obj_t) * ((pool_last - pool_first) + 1),
           sizeof(ae_obj_t) * ((pool_last - pool_first) + 1));
       
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
    NL;

    pool_print();

    NL;
    puts("Describing items in program.");
    if (CONSP(program_obj) && CAR(program_obj))
      EACH(program_obj, describe);
    puts("Described items in program.");
    NL;

    puts("Writing program obj.");
    ae_obj_write(program_obj);
    puts("\nWrote program obj.");
    NL;

    fputs("Count items in program obj: ", stdout); 
    fflush(stdout);
    NL;
    printf("%d", LENGTH(program_obj));
    NL;
    NL;
    fflush(stdout);

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

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE SYMBOL QUOTE CHAR LIST INF NILTOK
%start program

%%

program: sexps { root = $$; }

atom: CHAR | COMPARE | FLOAT | INTEGER | MATHOP | RATIONAL | STRING | SYMBOL | INF;

list:
LPAREN sexps RPAREN { $$ = $2; };
| LIST {
  INIT($$, AE_CONS);
}

sexp: list | atom

sexps:
sexps sexp {
  if (! $$) {
    printf("Beginning with ");
    PUT($2);
    putchar('\n');
    fflush(stdout);

    $$ = CONS($2, NIL);

    printf("Made ");
    PUT($$);
    putchar('\n');
    fflush(stdout);
  }
  else {
    printf("Embedding ");
    PUT($2);
    putchar('\n');
    fflush(stdout);

    PUSH($$, $2);

    printf("Made ");
    PUT($$);
    putchar('\n');
    fflush(stdout);

  }
}
| {
  $$ = NULL;
};
   
%%
