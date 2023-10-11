#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.inc"

//////////////////////////////////////////////////////////////////////////////
// describe
//////////////////////////////////////////////////////////////////////////////

void describe(ae_obj_t * this, bool dotted) {
  static unsigned int indent = 0;

  char buff[128] = { 0 };
  
  int written = 0;

  while (written++ < ((int)(indent - (dotted ? 2 : 0)))) SPC;

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

//////////////////////////////////////////////////////////////////////////////
// do_write
//////////////////////////////////////////////////////////////////////////////

void do_write(ae_obj_t * this) {
  ae_write(this);
  NL;
}

//////////////////////////////////////////////////////////////////////////////
// main
//////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv) {
  ae_obj_t * root_env = setup_root_env();
  
//////////////////////////////////////////////////////////////////////////////////////////////////
// read and parse file
//////////////////////////////////////////////////////////////////////////////////////////////////

  FILE * fp = fopen("data/sample.lisp", "r");
  yyin = fp;
  yyparse();
  fclose(fp);  

  paint_parsed();
  
//////////////////////////////////////////////////////////////////////////////////////////////////
// dump the pool, eval the program, dump the pool again
//////////////////////////////////////////////////////////////////////////////////////////////////  

#ifdef AE_DUMP_POOL_BEFORE
  pool_print();
#endif

  /* puts("Writing program obj."); */
  /* WRITE(program); */
  /* NL; */
  /* puts("Wrote program obj."); */
  /* NL; */
  
  /* SLOG("Evaluating program..."); */
  EVAL(root_env, program);
  /* SLOG("\nDone evaluating program.\n"); */
  
#ifdef AE_DUMP_POOL_AFTER
  pool_print();
#endif

#ifdef AE_DUMP_SYMS
  PR("syms: "); WRITE(ENV_SYMS(root_env)); NL;
#endif

#ifdef AE_DUMP_VALS
  PR("vals: "); WRITE(ENV_VALS(root_env)); NL;
#endif
}

//////////////////////////////////////////////////////////////////////////////////////////////////  
// End of main
//////////////////////////////////////////////////////////////////////////////////////////////////  
