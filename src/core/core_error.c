#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _error
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_error(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("error");

  REQUIRE(env, args, LENGTH(args) == 1 || LENGTH(args) == 2, "error requires 1 or 2 args");
  REQUIRE(env, args, STRINGP(CAR(args)), "error's 1st arg must be a string");

  ae_obj_t * err = NIL;
  
  if (LENGTH(args) == 2) {
    LOG(args, "error's args:");
    LOG(CDR(args), "error's args tail:");
    LOG(CADR(args), "error's args 2nd:");
    FF;
    REQUIRE(env, args, PROPERP(CADR(args)), "error's 2nd arg must be a list or nil");
    err = NEW_ERROR(STR_VAL(CAR(args)), CADR(args));
  }
  else {
    err = NEW_ERROR(STR_VAL(CAR(args)), NIL);
  }
  
  CORE_RETURN("error", err);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errmsg
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errmsg(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("errmsg");

  REQUIRE(env, args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  CORE_RETURN("errmsg", NEW_STRING(EMSG(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errobj(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("errobj");

  REQUIRE(env, args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  CORE_RETURN("errobj", EOBJ(CAR(args)));
}

