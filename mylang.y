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
  printf("Yac got sexps.\n");
};
sexp: QUOTE sexp | list | atom
{
  printf("Yac got sexp.\n");
};
list: LPAR sexps RPAR
{
  printf("Yac got list.\n");
};
atom: STRING | INTEGER | FLOAT | RATIONAL | MATHOP | COMPARE | WORD | CHAR
{
  printf("Yac got atom.\n");
};
%%
