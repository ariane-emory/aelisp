#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _error
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(error) {
  REQUIRE(STRINGP(CAR(args)), "error's 1st arg must be a string");
  REQUIRE((args_length == 1)  ||
          (PROPERP(CADR(args)) && ((LENGTH(CADR(args)) % 2) == 0)), "error's 2nd arg must be a proper list or nil");

  ae_obj_t * const err = NEW_ERROR(STR_VAL(CAR(args)));

  ASSIGN_PROPS(CADR(args), err);

  RETURN(err);

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _message
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(message) {
  REQUIRE(ERRORP(CAR(args)));

  RETURN(NEW_STRING(EMSG(CAR(args))));
  
  END_DEF_CORE_FUN;
}

