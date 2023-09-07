%{
#include <stdio.h>
#include <string.h>
#include "ae-data.h"

#define YAC_PRINT(x) printf("Yac got %s.\n", ae_object_str(&x));
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; } 
  main() { yyparse(); } 
%}

%token LBRC RBRC LSQR RSQR COMMA SEMI LPAR RPAR STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE WORD QUOTE CHAR

%%
sexps: | sexps sexp { printf("Yac got sexps.\n"); };

sexp:     QUOTE sexp | list | atom { printf("Yac got sexp.\n"); };
list:     LPAR sexps RPAR          { printf("Yac got list.\n"); };
string:   STRING                   { YAC_PRINT($1); };
integer:  INTEGER                  { YAC_PRINT($1); };
float:    FLOAT                    { YAC_PRINT($1); };
rational: RATIONAL                 { YAC_PRINT($1); };
mathop:   MATHOP                   { YAC_PRINT($1); };
compare:  COMPARE                  { YAC_PRINT($1); };
word:     WORD                     { YAC_PRINT($1); };
char:     CHAR                     { YAC_PRINT($1); };
atom:     string | integer | float | rational | mathop | compare | word | char;

%%
