%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"

#define YYSTYPE ae_obj_t

#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')
#define OBJ(x)  ae_obj_put(x)

  ae_obj_t * root = 0;

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }

  void describe(ae_obj_t * ae_obj) {
    static unsigned int indent = 0;
    
    ae_obj_t * this = ae_obj;

    for (int ct = 0; ct < indent << 1; ct++)
      SPC;
    
    OBJ(this);
    SPC;
    SPC;
    NL;
    
    if (this->type == AE_LIST && this->head) {
      ++indent;
      ae_obj_each(this, describe);
      --indent;
    }
  }

  extern FILE * yyin;
  extern ae_obj_t pool[POOL_SIZE];
  
  main() {
#define PRINT_SIZEOF(t)     printf("sizeof(" #t ") = %d bytes.\n", sizeof(t))
    PRINT_SIZEOF(int);
    PRINT_SIZEOF(ae_obj_t *);
    PRINT_SIZEOF(ae_obj_t);
    PRINT_SIZEOF(ae_type_t);

#ifdef USE_POOL
    printf("Using pool from %p to %p.\n", pool, &pool[POOL_SIZE]);
#endif
    
    FILE * fp = fopen("sample.txt", "r");
    yyin = fp;
    yyparse();

    printf("\nroot:                           ");
    ae_obj_put(root);
    NL;
    
    ae_obj_t * program_obj = ALLOC_AE_OBJ; 
    ae_obj_unsafe_move(program_obj, root); // take the 'program' rule's ae_obj.

    printf("program:                        ");
    ae_obj_put(program_obj);
    NL;
    NL;
    if (program_obj->type == AE_LIST && program_obj->head)
      ae_obj_each(program_obj, describe);
    puts("Done loop.");
    fflush(stdout);
    NL;
    NL;
    puts("Writing...");
    fflush(stdout);
    ae_obj_write(program_obj);
    NL;    
    write(program_obj);
    NL;
  }
    
    %}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE SYMBOL QUOTE CHAR LIST
%start program

%%

program: sexps { root = &$$; }

sexp: list | atom

atom: CHAR | COMPARE | FLOAT | INTEGER | MATHOP | RATIONAL | STRING | SYMBOL;

list:  LIST
{
  printf("Initting $$ @ %p.\n", &$$);
  ae_obj_init(&$$, AE_LIST);
  printf("Done initting $$ @ %p.\n", &$$);
} | LPAREN sexps RPAREN { $$ = $2; };

sexps: sexps sexp
{
  ae_obj_t * new_obj = ALLOC_AE_OBJ;
  ae_obj_unsafe_move(new_obj, &$2);
  ae_obj_push_back(&$$, new_obj);
} | {
  printf("Initting $$ @ %p.\n", &$$);
  ae_obj_init(&$$, AE_LIST);
  printf("Done initting $$ @ %p.\n", &$$);
};
   
%%
