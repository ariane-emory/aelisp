#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_write(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("write");

  int written = 0;

  FOR_EACH(elem, args) {
    written += WRITE(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  CORE_RETURN("write", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("put");

  int written = 0;

  FOR_EACH(elem, args)
    written += PUT(elem);

  fflush(stdout);

  CORE_RETURN("put", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("princ");

  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  CORE_RETURN("princ", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
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

  CORE_RETURN("print", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nl
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nl(__attribute__((unused)) ae_obj_t * const env,
                      __attribute__((unused)) ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_BEGIN("nl");
  NL;
  CORE_RETURN("nl", NIL);
}

