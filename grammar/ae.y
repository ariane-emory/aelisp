%{

#include <stdio.h>

#include "common.h"
#include "free_list.h"
#include "obj.h"
#include "list.h"
#include "log.h"

#define YYSTYPE ae_obj_t *

#ifdef AE_READ_FILE_OBJ_TAGGING
// This will need to be revised to deal with the upcoming load stack.
#  define TAG(o) (PUT_PROP(NEW_INT(yylineno + 1), "line", ((o))), PUT_PROP(filename_stack, "file", ((o))))
#else
#  define TAG(o) (o)
#endif

extern int yylineno;
extern ae_obj_t * program;
extern int main(int argc, char ** argv);

void yyerror(const char *str) {
    if (!NILP(filename_stack))
        ERR("Error on line %d of %s: %s\n", yylineno + 1, STR_VAL(CAR(filename_stack)), str);
    else
        ERR("Error on line %d: %s\n", yylineno + 1, str);

    read_error = true;
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
    $$ = $2;
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
