%{

#include <stdio.h>

#include "obj.h"
#include "list.h"
#include "util.h"
#include "free_list.h"

#define YYSTYPE ae_obj_t *

#define TAG(o) (PUT_PROP(((o)), "line", NEW_INT(yylineno)), PUT_PROP(((o)), "file", last_loaded_file))

extern int yylineno;
extern ae_obj_t * program;
extern int main(int argc, char ** argv);
extern ae_obj_t * last_loaded_file;

void yyerror(const char *str) {
    if (!NILP(last_loaded_file))
        ERR("Error on line %d of %s: %s\n", yylineno, STR_VAL(last_loaded_file), str);
    else
        ERR("Error on line %d: %s\n", yylineno, str);
}

int  yywrap() { return 1; }   

%}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR INF NILTOK DOT BACKTICK COMMA COMMA_AT POUND AT DOLLAR

%start program

%%

sexp: atom | list | quoted_sexp | lit_list_sexp |  quasiquoted_sexp | unquoted_sexp | spliced_sexp;
atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

program: sexps                                  { program = CONS(SYM("progn"), $$); };
sexps:            sexp     sexps                { $$      = CONS($1, $2); } | { $$ = NIL; };
list:             LPAREN   list_elements RPAREN {
    $$      = $2;
//    PUT_PROP($$, SYM("file"), NEW_STR(last_loaded_file));
    TAG($$);
    /* WRITE($$); */
    /* SPC; */
    /* WRITE(PROPS($$)); */
    /* NL; */
};

list_elements:    sexp     list_elements        { $$      = CONS($1, $2); } | sexp DOT sexp { $$ = NEW_CONS($1, $3); } | { $$ = NIL; };

quoted_sexp:      QUOTE    sexp                 { $$      = CONS(SYM("quote"),            CONS($2, NIL)); TAG($$); };
quasiquoted_sexp: BACKTICK sexp                 { $$      = CONS(SYM("quasiquote"),       CONS($2, NIL)); TAG($$); };
unquoted_sexp:    COMMA    sexp                 { $$      = CONS(SYM("unquote"),          CONS($2, NIL)); TAG($$); };
spliced_sexp:     COMMA_AT sexp                 { $$      = CONS(SYM("unquote-splicing"), CONS($2, NIL)); TAG($$); };
lit_list_sexp:    DOLLAR   sexp                 { $$      = CONS(SYM("list"),             $2);            TAG($$); };

%%
