%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ae.h"

  ae_object_t * root = 0;

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }

  void describe(void * ae_object) {
    printf("0x%zu -> %s\n", ae_object, ae_object_str(ae_object));
  }
  
  main() {
    yyparse();

    printf("Root: %s.\n", ae_object_str(root));

    ae_object_t * root_object = malloc(sizeof(ae_object_t));
    ae_object_move(root_object, root);
    
    ae_list_t first_list = root_object->data.list_value;
    ae_list_node_t first_lists_first_node = *root_object->data.list_value;
    
    printf("Root's list_value: %s.\n", ae_list_str(&first_list));
    printf("Root's list_value len: %d.\n", ae_list_length(&first_list));
    printf("Root's list_value len: %d.\n", ae_list_node_length(&first_lists_first_node));

    ae_object_t * obj = first_lists_first_node.object;
    printf("Obj: %s.\n", ae_object_str(obj));
    printf("Obj's list_value: %s.\n", ae_list_str(&obj->data.list_value));
    printf("Obj's list_value len: %d.\n", ae_list_length(&obj->data.list_value));

    ae_list_each(&first_list, describe);
  }
    
    %}

%token LPAR RPAR STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE SYMBOL QUOTE CHAR
%start program

%%

program: sexps { root = &$$; }

sexp: list | atom

atom: CHAR | COMPARE | FLOAT | INTEGER | MATHOP | RATIONAL | STRING | SYMBOL;

list:  LPAR sexps RPAR { $$ = $2; };

sexps: sexps sexp
{
  printf("\nYacc cont'd sexps. Copied 0x%zu %s.\n", &$1, ae_object_str(&$1));
  ae_object_t * new_object = malloc(sizeof(ae_object_t));
  ae_object_move(new_object, &$2);
  printf("Yacc cont'd sexps. Pushing 0x%zu %s.\n", new_object, ae_object_str(new_object));
  ae_list_push_back(&$$.data.list_value, new_object);
  printf("Yacc cont'd sexps. Returning 0x%zu %s.\n", &$$, ae_object_str(&$$));
} | {
  ae_object_init(&$$);
  $$.type = AE_LIST;
  ae_list_init(&$$.data.list_value);
  printf("\nYacc began sexps. Created 0x%zu %s.\n", &$$, ae_object_str(&$$));
};
   
%%
