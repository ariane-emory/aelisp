#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_write(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("write");

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
  
end:
  
  CORE_EXIT("write");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_ENTER("put");

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
  
end:
  
  CORE_EXIT("put");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("princ");

  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  RETURN(NEW_INT(written));
  
end:
  
  CORE_EXIT("princ");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("print");

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

end:
  
  CORE_EXIT("print");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nl
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nl(__attribute__((unused)) ae_obj_t * const env,
                      __attribute__((unused)) ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_ENTER("nl");

  NL;

  RETURN(NEW_INT(1));

end:
  
  CORE_EXIT("nl");
}

