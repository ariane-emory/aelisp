%{
#include <stdio.h>

#include "ae_obj.h"
#include "ae_list.h"
#include "ae_util.h"

#define YYSTYPE ae_obj_t *

extern ae_obj_t * program;
extern int main(void);
  
void yyerror(const char *str) { ERR("Error: %s\n", str); }
int  yywrap() { return 1; }    
%}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR INF NILTOK DOT BACKTICK COMMA COMMA_AT

%start program

%%

sexp: atom | list | quoted_sexp | quasiquoted_sexp | unquoted_sexp | spliced_sexp;
atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

program: sexps                               { program = CONS(SYM("progn"), $$); };
sexps:            sexp     sexps             { $$      = CONS($1, $2); } | { $$ = NIL; };
list:             LPAREN   list_sexps RPAREN { $$      = $2; };
list_sexps:       sexp     list_sexps        { $$      = CONS($1, $2); } | sexp DOT sexp { $$ = NEW_CONS($1, $3); } | { $$ = NIL; };

quoted_sexp:      QUOTE    sexp              { $$      = CONS(SYM("quote"),      CONS($2, NIL)); };
quasiquoted_sexp: BACKTICK sexp              { $$      = CONS(SYM("quasiquote"), CONS($2, NIL)); };
unquoted_sexp:    COMMA    sexp              { $$      = CONS(SYM("unquote"),    CONS($2, NIL)); };
spliced_sexp:     COMMA_AT sexp              { $$      = CONS(SYM("splice"),     CONS($2, NIL)); };

%%
