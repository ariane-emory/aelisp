%{
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"
#include "ae_env.h"
#include "ae_eval.h"
#include "ae_free_list.h"
#include "ae_write.h"
#include "ae_util.h"

#define YYSTYPE ae_obj_t *

#ifdef AE_LOG_PARSE
#  define LOG_PARSE(obj, ...)                                                                      \
  printf(__VA_ARGS__);                                                                             \
  if (obj)                                                                                         \
    PUT((obj));                                                                                    \
  else                                                                                             \
    PR("NULL!");                                                                                   \
  putchar(' ');                                                                                    \
  putchar('\n');                                                                                   \
  fflush(stdout)
#else
#  define LOG_PARSE(obj, ...) ((void)(obj))
#endif

  ae_obj_t * root    = 0;
  
#define free_list_size (1 << 16)

  static char mem[free_list_size] = { 0 };
  
  void yyerror(const char *str) { fprintf(stderr, "Error: %s\n", str); }

  int  yywrap() { return 1; }

  void describe(ae_obj_t * this, bool dotted) {
    static unsigned int indent = 0;

    char buff[128] = { 0 };
    
    int written = 0;

    while (written++ < (indent - (dotted ? 2 : 0))) SPC;

    if (dotted)
      written += PR("• "); 
    
    written += PR("%018p", this);
    
    while (written++ < 32) SPC;

    // This hacky extra print is required because we printed a multi-byte character earlier:
    if (dotted)
      written += PR("  ");
    
    written += PUT(this);
    
    while (written++ < 105) SPC;

    // PUT_words(this);

    NL;
    
    if (CONSP(this)) {
      indent += 2;
      FOR_EACH(elem, this) {
        describe(elem, false);
        if (! TAILP(CDR(position)))
          describe(CDR(position), true);
      }
      indent -= 2;
    }
  }

  void do_write(ae_obj_t * this) {
    ae_write(this);
    NL;
  }

  extern FILE * yyin;

  //////////////////////////////////////////////////////////////////////////////
  // preface
  //////////////////////////////////////////////////////////////////////////////

  void preface(void) {
    NL;
    printf("obj size:          %d.\n",    sizeof(ae_obj_t));
    printf("int size:          %d.\n",    sizeof(int));
    printf("nil is at:         %016p.\n", NIL);
    printf("t is at:           %016p.\n", TRUE);
    printf("Pool first:        %016p.\n", pool_first);
    printf("Pool last:         %016p.\n", pool_last);
    printf("Pool size:         %016p (%zu bytes).\n",
           sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE,
           sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE);
    printf("Strings pool size: %016p (%zu bytes).", free_list_size, free_list_size);
    NL;
  }

  //////////////////////////////////////////////////////////////////////////////
  // describe_parse
  //////////////////////////////////////////////////////////////////////////////

  void describe_parse(ae_obj_t * program_obj) {
    /* printf("\nprogram:           "); */

    /* if (! program_obj) { */
    /*   PR("NULL!"); */
    /*   return; */
    /* } */
    /* else { */
    /*   PUT(CADR(program_obj)); */
    /* } */
    
/* #ifdef AE_DUMP_POOL_BEFORE */
/*     NL; */
/*     pool_print(); */
/*     NL; */
/* #endif */

    /* puts("\nDescribing items in program."); */
    /* FOR_EACH(obj, CONS(program_obj, NIL)) */
    /*   describe(obj, false); */
    /* puts("Described items in program."); */
    /* NL; */

    /* fputs("Count items in program obj: ", stdout);  */
    /* printf("%d", LENGTH(CDR(program_obj))); */
    /* NL; */
    /* NL; */
    
    /* puts("Writing items in program obj."); */
    /* if (CONSP(program_obj) && CDR(program_obj)) */
    /*   EACH(CDR(program_obj), do_write); */
    /* puts("Wrote items in program obj."); */
    /* NL; */

    puts("Writing program obj.");
    WRITE(program_obj);
    NL;
    puts("Wrote program obj.");
    NL;
    
    /* puts("Writing interned symbols."); */
    /* ae_write(symbols_list); */
    /* puts("\nWrote interned symbols.");     */
  }
  
  //////////////////////////////////////////////////////////////////////////////
  // main
  //////////////////////////////////////////////////////////////////////////////

  main() {
    preface();
    
    free_list_add_block(&mem[0], free_list_size);
    
    PR("\nPopulating root env...");
    INDENT;
    ae_obj_t * env = ENV_NEW_ROOT();
    OUTDENT;
    LOG(env, "Done populating");


    FILE * fp = fopen("data/sample.lisp", "r");
    yyin = fp;
    yyparse();

#ifdef AE_DUMP_POOL_BEFORE
    pool_print();
#endif

    ae_obj_t * program_obj = CONS(SYM("progn"), root);

    describe_parse(program_obj);

    SLOG("\nEvaluating program...");
    EVAL(env, program_obj);
    SLOG("\nDone evaluating program.\n");

#ifdef AE_DUMP_POOL_AFTER
    pool_print();
#endif
    
    fclose(fp);

  }

    //////////////////////////////////////////////////////////////////////////////
    // End of main
    //////////////////////////////////////////////////////////////////////////////
    
    %}

%token LPAREN RPAREN STRING INTEGER FLOAT RATIONAL SYMBOL QUOTE CHAR INF NILTOK DOT BACKTICK COMMA COMMA_AT

%start program

%%

program: sexps { root = $$; }
sexp: atom | list | quoted_sexp | quasiquoted_sexp | unquoted_sexp | spliced_sexp;
atom: CHAR | FLOAT | INTEGER | RATIONAL | STRING | SYMBOL | INF;

quoted_sexp:      QUOTE    sexp              { $$ = CONS(SYM("quote"),      CONS($2, NIL)); };
quasiquoted_sexp: BACKTICK sexp              { $$ = CONS(SYM("quasiquote"), CONS($2, NIL)); };
unquoted_sexp:    COMMA    sexp              { $$ = CONS(SYM("unquote"),    CONS($2, NIL)); };
spliced_sexp:     COMMA_AT sexp              { $$ = CONS(SYM("splice"),     CONS($2, NIL)); };

sexps:            sexp     sexps             { $$ = CONS($1, $2); } | { $$ = NIL; };
list:             LPAREN   list_sexps RPAREN { $$ = $2; };
list_sexps:       sexp     list_sexps        { $$ = CONS($1, $2); } | sexp DOT sexp { $$ = NEW_CONS($1, $3); } | { $$ = NIL; };


%%
