#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _error
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_error(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("error");

  REQUIRE(env, args, STRINGP(CAR(args)), "error's 1st arg must be a string");
  REQUIRE(env, args,
          (args_length == 1)  ||
          (PROPERP(CADR(args)) && ((LENGTH(CADR(args)) % 2) == 0)), "error's 2nd arg must be a proper list or nil");

  ae_obj_t * const err = NEW_ERROR(STR_VAL(CAR(args)));

  ASSIGN_PROPS(CADR(args), err);

  RETURN(err);

  CORE_END("error");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _message
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_message(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("message");

  REQUIRE(env, args, ERRORP(CAR(args)));

  RETURN(NEW_STRING(EMSG(CAR(args))));
  
  CORE_END("message");
}

