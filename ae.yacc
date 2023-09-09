%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae.h"

// Utility macros that should probably be moved to another file and/or renamed/undefed.
  
#define NL      putchar('\n')
#define BSPC    putchar('\b')
#define SPC     putchar(' ')
#define LPAR    putchar('(')
#define RPAR    putchar(')')
#define LSQR    putchar('[')
#define RSQR    putchar(']')
#define OBJ(x)  ae_object_puts(x)
#define OBJC(x) ae_object_putsc(x)
  
#define POOL_SIZE (1 << 12)

  ae_object_t pool[POOL_SIZE];
  ae_object_t * root = 0;

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }

  void describe(void * ae_object) {
    static unsigned int indent = 0;
    
    ae_object_t * this = ae_object;

    for (int ct = 0; ct < indent << 1; ct++)
      SPC;
    
    OBJ(this);
    SPC;
    SPC;
    // LSQR;
    // OBJC(this);
    // RSQR;
    NL;
    
    if (this->type == AE_LIST) {
      ++indent;
      ae_list_each(&this->data.list_value, describe);
      --indent;
    }
  }

  void write(void * ae_object) {
    ae_object_t * this = ae_object;
        
    if (this->type == AE_LIST) {
      LPAR;
      if (this->data.list_value) {
        ae_list_each(&this->data.list_value, write);
        BSPC;
      }
      RPAR;
      SPC;
    }
    else {
      ae_object_putsc(this);
      SPC;
    }
  }

  ae_object_t * pool_alloc_ae_object() {
    for (size_t ix = 0; ix < POOL_SIZE; ix++) {
      ae_object_t * obj = &pool[ix];

      if (obj->type != AE_FREE)
        continue;
      
      ae_object_init(obj);
      obj->type = AE_INVALID;

      #define BUFF_LEN 5
      char buff[BUFF_LEN] = { 0 };
      snprintf(buff, BUFF_LEN, "#%d:", ix); // off by one? I forget.
      #undef BUFF_LEN
      
      printf("Pool allocated instance %-6s  %s.\n", buff, ae_object_str(obj));
      return obj;
    }
    
    printf("ERROR: Pool is full.\n");
    return 0;
  }

  void pool_free_ae_object(ae_object_t * const this) {
    ae_object_init(this);
    this->type = AE_FREE;
  }

#define USE_POOL
  
#ifdef USE_POOL
#  define ALLOC_AE_OBJECT pool_alloc_ae_object()
#else
#  define ALLOC_AE_OBJECT malloc(sizeof(ae_object_t))
#endif
  
  main() {
#define PRINT_SIZEOF(t)     printf("sizeof(" #t ") = %d bytes.\n", sizeof(t))
    PRINT_SIZEOF(int);
    PRINT_SIZEOF(void *);
    PRINT_SIZEOF(ae_data_t);
    PRINT_SIZEOF(ae_list_t);
    PRINT_SIZEOF(ae_list_node_t);
    PRINT_SIZEOF(ae_object_t);
    PRINT_SIZEOF(ae_rational_t);
    PRINT_SIZEOF(ae_type_t);
    
    yyparse();

    printf("\nroot:                           %s.\n", ae_object_str(root));

    ae_object_t * program_object = ALLOC_AE_OBJECT; 
    ae_object_move(program_object, root); // take the 'program' rule's ae_object.

    printf("program:                        %s.\n", ae_object_str(program_object));
    NL;

    ae_list_each(&program_object->data.list_value, describe);
    NL;
    
    ae_list_each(&program_object->data.list_value, write);
    NL;
    
    printf("bing.\n");
    write(&program_object);
    printf("bong.\n");
  }
    
    %}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE SYMBOL QUOTE CHAR
%start program

%%

program: sexps { root = &$$; }

sexp: list | atom

atom: CHAR | COMPARE | FLOAT | INTEGER | MATHOP | RATIONAL | STRING | SYMBOL;

list:  LPAREN sexps RPAREN { $$ = $2; };

sexps: sexps sexp
{
  printf("\nYacc cont'd sexps. Copied       %s.\n", ae_object_str(&$1));
  ae_object_t * new_object = ALLOC_AE_OBJECT;
  ae_object_move(new_object, &$2);
  printf("Yacc cont'd sexps. Pushing      %s.\n", ae_object_str(new_object));
  ae_list_push_back(&$$.data.list_value, new_object);
  printf("Yacc cont'd sexps. Returning    %s.\n", ae_object_str(&$$));
} | {
  ae_object_init(&$$);
  $$.type = AE_LIST;
  ae_list_init(&$$.data.list_value);
  printf("\nYacc began sexps.  Created      %s.\n", ae_object_str(&$$));
};
   
%%
