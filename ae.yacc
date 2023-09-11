%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_list.h"
#include "ae_object.h"

#define YYSTYPE ae_obj_t

// Utility macros that should probably be moved to another file and/or renamed/undefed.
  
#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')
#define OBJ(x)  ae_obj_puts(x)
#define OBJC(x) ae_obj_putsc(x)
  
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
      ae_list_each(&this->list_value, describe);
      --indent;
    }
  }

  void write(ae_obj_t * ae_obj) {
    ae_obj_t * this = ae_obj;
        
    if (this->type == AE_LIST) {
      if (this->list_value) {
        LPAR;
        ae_list_each(&this->list_value, (ae_list_node_each_fun)write);
        BSPC;
        RPAR;
      }
      else {
        fputs("nil", stdout);
      }
      SPC;
    }
    else {
      ae_obj_putsc(this);
      SPC;
    }
  }

  ae_obj_t * pool_alloc_ae_obj() {
    for (size_t ix = 0; ix < POOL_SIZE; ix++) {
      ae_obj_t * obj = &pool[ix];

      if (obj->type != AE_FREE)
        continue;
      
      ae_obj_init(obj);
      obj->type = AE_INVALID;

      #define BUFF_LEN 5
      char buff[BUFF_LEN] = { 0 };
      snprintf(buff, BUFF_LEN, "#%d:", ix); // off by one? I forget.
      #undef BUFF_LEN
      
      printf("Pool allocated instance %-6s  %s.\n", buff, ae_obj_str(obj));
      return obj;
    }
    
    printf("ERROR: Pool is full.\n");
    return 0;
  }

  void pool_free_ae_obj(ae_obj_t * const this) {
    ae_obj_init(this);
    this->type = AE_FREE;
  }

#define USE_POOL
  
#ifdef USE_POOL
#  define ALLOC_AE_OBJ pool_alloc_ae_obj()
#else
#  define ALLOC_AE_OBJ malloc(sizeof(ae_obj_t))
#endif
  
  main() {
#define PRINT_SIZEOF(t)     printf("sizeof(" #t ") = %d bytes.\n", sizeof(t))
    PRINT_SIZEOF(int);
    PRINT_SIZEOF(void *);
    PRINT_SIZEOF(ae_list_t);
    PRINT_SIZEOF(ae_list_node_t);
    PRINT_SIZEOF(ae_obj_t);
    PRINT_SIZEOF(ae_type_t);
    
    yyparse();

    printf("\nroot:                           %s.\n", ae_obj_str(root));

    ae_obj_t * program_obj = ALLOC_AE_OBJ; 
    ae_obj_move(program_obj, root); // take the 'program' rule's ae_obj.

    printf("program:                        %s.\n", ae_obj_str(program_obj));
    NL;

    ae_list_each(&program_obj->list_value, describe);
    NL;
    
    ae_list_each(&program_obj->list_value, (ae_list_node_each_fun)write);
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
  ae_obj_init(&$$);
  $$.type = AE_LIST;
  ae_list_init(&$$.list_value);
} | LPAREN sexps RPAREN { $$ = $2; };

sexps: sexps sexp
{
  printf("\nYacc cont'd sexps. Copied       %s.\n", ae_obj_str(&$1));
  ae_obj_t * new_obj = ALLOC_AE_OBJ;
  ae_obj_move(new_obj, &$2);
  printf("Yacc cont'd sexps. Pushing      %s.\n", ae_obj_str(new_obj));
  ae_list_push_back(&$$.list_value, new_obj);
  printf("Yacc cont'd sexps. Returning    %s.\n", ae_obj_str(&$$));
} | {
  ae_obj_init(&$$);
  $$.type = AE_LIST;
  ae_list_init(&$$.list_value);
  printf("\nYacc began sexps.  Created      %s.\n", ae_obj_str(&$$));
};
   
%%
