%{
#include <stdio.h>
#include <string.h>
#include "ae.h"

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }
  main() { yyparse(); }
%}

%token LPAR RPAR STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE WORD QUOTE CHAR
%start sexps

%%

sexp:
list   { /* printf("Yacc got sexp (list) and returns %s.\n", ae_object_str(&$$)); */ };
| atom { /* printf("Yacc got sexp (atom) and returns %s.\n", ae_object_str(&$$)); */ };

list:  LPAR sexps RPAR
{
  $$ = $2;
  printf("Yacc got list and returns %s.\n", ae_object_str(&$$));
};

sexps: sexps sexp
{
  printf("\nYacc cont'd sexps. Copied %s.\n", ae_object_str(&$1));
  printf("Yacc cont'd sexps. Pushing %s.\n", ae_object_str(&$2));
  ae_list_push_back(&$1.data.list_value, &$2);
  $$ = $1;
  printf("Yacc cont'd sexps. Returning %s.\n", ae_object_str(&$1));
}
| {
  ae_object_init(&$$);
  $$.type = ML_LIST;
  ae_list_init(&$$.data.list_value);
  printf("\nYacc began sexps. Created %s.\n", ae_object_str(&$$));
};
   
atom:
char;
| compare 
| float 
| integer  
| mathop 
| rational 
| string 
| word 

char:     CHAR;
compare:  COMPARE;
float:    FLOAT;
integer:  INTEGER;
mathop:   MATHOP;
rational: RATIONAL;
string:   STRING;
word:     WORD;

%%
