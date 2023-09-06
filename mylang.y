%{
#include <stdio.h>
#include <string.h>
#include "mylang-data.h"
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; } 
  main() { yyparse(); } 

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

atom: string | integer | float | rational | mathop | compare | word | char

string: STRING
{
  printf("Yac got string.\n");
};

integer: INTEGER
{
  // struct mylang_object_t obj = $1;
  printf("Yac got integer: '%s'.\n", mylang_object_str(&$1));
};

float: FLOAT
{
  printf("Yac got float.\n");
};

rational: RATIONAL
{
  printf("Yac got rational.\n");
};

mathop: MATHOP
{
  printf("Yac got mathop.\n");
};

compare: COMPARE
{
  printf("Yac got compare.\n");
};

word: WORD
{
  printf("Yac got word.\n");
};

char: CHAR
{
  printf("Yac got char.\n");
};

%%
