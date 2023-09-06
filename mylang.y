%{
#include <stdio.h>
#include <string.h>
#include "mylang-data.h"
  
void yyerror(const char *str) {
  fprintf(stderr, "Error: %s\n", str);
}

int yywrap() {
  return 1;
} 

main() {
  yyparse();
} 

%}

%token LBRC RBRC LSQR RSQR COMMA SEMI LPAR RPAR STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE WORD QUOTE CHAR

%%
sexps: | sexps sexp
{
  printf("Got sexps.\n");
};
sexp: QUOTE sexp | list | atom
{
  printf("Got sexp.\n");
};
list: LPAR sexps RPAR
{
  printf("Got list.\n");
};
atom: STRING | INTEGER | FLOAT | RATIONAL | MATHOP | COMPARE | WORD | CHAR
{
  printf("Got atom.\n");
};
%%
