%{
#include <stdio.h>
#include <string.h>
#include "ae.h"

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }
  main() { yyparse(); }

  ae_object_t * root = 0;
    
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
  printf("\nYacc cont'd sexps. Copied %s.\n", ae_object_str(&$1));
  printf("Yacc cont'd sexps. Pushing %s.\n", ae_object_str(&$2));
  ae_list_push_back(&$$.data.list_value, &$2);
  printf("Yacc cont'd sexps. Returning %s.\n", ae_object_str(&$$));
} | {
  ae_object_init(&$$);
  $$.type = AE_LIST;
  ae_list_init(&$$.data.list_value);
  printf("\nYacc began sexps. Created %s.\n", ae_object_str(&$$));
};
   
%%
