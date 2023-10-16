%{

#include <stdio.h>

#include "obj.h"
#include "list.h"
#include "util.h"

#define YYSTYPE ae_obj_t *

extern int yylineno;
extern ae_obj_t * program;
extern int main(int argc, char ** argv);
  
void yyerror(const char *str) { ERR("Error on line %d: %s\n", yylineno, str); }
int  yywrap() { return 1; }    

%}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR INF NILTOK DOT BACKTICK COMMA COMMA_AT AT

%start program

%%

sexp: atom | list | quoted_sexp | quasiquoted_sexp | unquoted_sexp | spliced_sexp | short_list_sexp;
atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

program: sexps                                  { program = CONS(SYM("progn"), $$); };
sexps:            sexp     sexps                { $$      = CONS($1, $2); } | { $$ = NIL; };
list:             LPAREN   list_elements RPAREN { $$      = $2; };
list_elements:    sexp     list_elements        { $$      = CONS($1, $2); } | sexp DOT sexp { $$ = NEW_CONS($1, $3); } | { $$ = NIL; };

quoted_sexp:      QUOTE    sexp                 { $$      = CONS(SYM("quote"),      CONS($2, NIL)); };
quasiquoted_sexp: BACKTICK sexp                 { $$      = CONS(SYM("quasiquote"), CONS($2, NIL)); };
unquoted_sexp:    COMMA    sexp                 { $$      = CONS(SYM("unquote"),    CONS($2, NIL)); };
spliced_sexp:     COMMA_AT sexp                 { $$      = CONS(SYM("splice"),     CONS($2, NIL)); };
short_list_sexp:  AT       sexp                 { $$      = CONS(SYM("list"),       CONS($2, NIL)); };

%%
