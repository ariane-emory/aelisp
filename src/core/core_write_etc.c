#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(write) {
  int written = 0;

  FOR_EACH(elem, args) {
    written += WRITE(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  RETURN(NEW_INT(written));
  
  END_DEF_CORE_FUN(write);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("put");

  int written = 0;

  FOR_EACH(elem, args) {
    written += PUT(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  RETURN(NEW_INT(written));
  
  CORE_END("put");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(__attribute__((unused))ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("princ");

  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  RETURN(NEW_INT(written));
  
  CORE_END("princ");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("print");

  int written = 0;

  FOR_EACH(elem, args) {
    written += PRINT(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  RETURN(NEW_INT(written));

  CORE_END("print");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nl
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nl(__attribute__((unused)) ae_obj_t * const env,
                      __attribute__((unused)) ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_BEGIN("nl");

  NL;

  RETURN(NEW_INT(1));

  CORE_END("nl");
}

