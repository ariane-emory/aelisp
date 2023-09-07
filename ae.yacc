%{
#include <stdio.h>
#include <string.h>
#include "ae.h"

#define YACC_PRINT(x) { printf("Yac got %s.\n", ae_object_str(&x)); }

  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }
  int yywrap() { return 1; }
  main() { yyparse(); }
    %}

%token LPAR RPAR STRING INTEGER FLOAT RATIONAL MATHOP INCROP COMPARE WORD QUOTE CHAR
%start sexps

%%


sexp:
list                               { $$ = $1; printf("Yac got sexp (list).\n"); };
| atom                             { $$ = $1; printf("Yac got sexp (atom).\n"); };

list:  LPAR sexps RPAR             { $$ = $1; printf("Yac got list.\n"); };

sexps: sexps sexp                  {
                                     printf("Yac got sexps (1).\n");
                                     $$ = $1;
                                     printf("Copied %s.\n", ae_object_str(&$2));
                                     char * nll = 0;
                                     ae_list_push_back(&$1.data.list_value, &$2);
                                   }
     |                             {
                                     printf("Yac got sexps (2).\n");
                                     ae_object_init(&$$);
                                     $$.type = ML_LIST;
                                     ae_list_init(&$$.data.list_value);
                                   };
   
atom:
string                             { $$ = $1; }
| integer                          { $$ = $1; }
| float                            { $$ = $1; }
| rational                         { $$ = $1; }
| mathop                           { $$ = $1; }
| compare                          { $$ = $1; }
| word                             { $$ = $1; }
| char                             { $$ = $1; };

string:   STRING                   { $$ = $1; YACC_PRINT($1); };
integer:  INTEGER                  { $$ = $1; YACC_PRINT($1); };
float:    FLOAT                    { $$ = $1; YACC_PRINT($1); };
rational: RATIONAL                 { $$ = $1; YACC_PRINT($1); };
mathop:   MATHOP                   { $$ = $1; YACC_PRINT($1); };
compare:  COMPARE                  { $$ = $1; YACC_PRINT($1); };
word:     WORD                     { $$ = $1; YACC_PRINT($1); };
char:     CHAR                     { $$ = $1; YACC_PRINT($1); };

%%
