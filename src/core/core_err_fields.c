#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _error
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_error(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("error");

  REQUIRE(env, args, CAR(args) && NILP(CADDR(args)), "error requires 1 or 2  args");
  REQUIRE(env, args, STRINGP(CAR(args)), "error's 1st arg must be a string");
  
  if (! TAILP(CADR(args)))
    REQUIRE(env, args, CONSP(CADR(args)), "error's 2nd arg must be a list");

  ae_obj_t * const err = NEW_ERROR(CADR(args), STR_VAL(CAR(args)));

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

