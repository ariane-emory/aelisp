%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_list.h"
#include "ae_obj.h"

#define YYSTYPE ae_obj_t

// Utility macros that should probably be unsafe_moved to another file and/or renamed/undefed.
  
#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')
#define OBJ(x)  ae_obj_put(x)
  
#define POOL_SIZE (1 << 12)

  ae_obj_t pool[POOL_SIZE];
  ae_obj_t * root = 0;

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }

  void describe(void * ae_obj) {
    static unsigned int indent = 0;
    
    ae_obj_t * this = ae_obj;

    for (int ct = 0; ct < indent << 1; ct++)
      SPC;
    
    OBJ(this);
    SPC;
    SPC;
    NL;
    
    if (this->type == AE_LIST) {
      ++indent;
      ae_obj_list_each(&this->list_value, describe);
      --indent;
    }
  }

  ae_obj_t * pool_alloc_ae_obj() {
    for (size_t ix = 0; ix < POOL_SIZE; ix++) {
      ae_obj_t * obj = &pool[ix];

      if (obj->type != AE_FREE)
        continue;
      
      ae_obj_init(obj, AE_INVALID);

      #define BUFF_LEN 5
      char buff[BUFF_LEN] = { 0 };
      snprintf(buff, BUFF_LEN, "#%d:", ix); // off by one? I forget.
      #undef BUFF_LEN
      
      return obj;
    }
    
    printf("ERROR: Pool is full.\n");
    return 0;
  }

  void pool_free_ae_obj(ae_obj_t * const this) {
    ae_obj_init(this, AE_FREE);
  }

#define USE_POOL
  
#ifdef USE_POOL
#  define ALLOC_AE_OBJ pool_alloc_ae_obj()
#else
#  define ALLOC_AE_OBJ malloc(sizeof(ae_obj_t))
#endif

  extern FILE * yyin;
  
  main() {
#define PRINT_SIZEOF(t)     printf("sizeof(" #t ") = %d bytes.\n", sizeof(t))
    PRINT_SIZEOF(int);
    PRINT_SIZEOF(void *);
    PRINT_SIZEOF(ae_list_t);
    PRINT_SIZEOF(ae_node_t);
    PRINT_SIZEOF(ae_obj_t);
    PRINT_SIZEOF(ae_type_t);
    
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
    ae_obj_list_each(&program_obj->list_value, describe);
    puts("Done loop.");
    fflush(stdout);
    NL;
    NL;
    puts("Writing...");
    fflush(stdout);
    ae_obj_write(program_obj);
    NL;    
    write(program_obj);
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
  ae_obj_init(&$$, AE_LIST);
  ae_list_init(&$$.list_value);
} | LPAREN sexps RPAREN { $$ = $2; };

sexps: sexps sexp
{
  ae_obj_t * new_obj = ALLOC_AE_OBJ;
  ae_obj_unsafe_move(new_obj, &$2);
  ae_list_push_back(&$$.list_value, new_obj);
} | {
  ae_obj_init(&$$, AE_LIST);
  ae_list_init(&$$.list_value);
};
   
%%
