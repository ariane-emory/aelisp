#include "core_includes.h"


////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("macro");

  REQUIRE(env, args, TAILP(CAR(args)) || SYMBOLP(CAR(args)));
  REQUIRE(env, args, TAILP(CDR(args)));

  // symbols used in params must not be '*special*':
  if (SYMBOLP(CAR(args))) {
      REQUIRE(env, args, ! SPECIAL_SYMP(CAR(args)), "special symbols cannot be used as params");
  }
  else {
    FOR_EACH (param, CAR(args)) {
      REQUIRE(env, args, SYMBOLP(param));
      REQUIRE(env, args, ! SPECIAL_SYMP(param), "special symbols cannot be used as params");
    }
  }

  RETURN(NEW_MACRO(CAR(args), CDR(args), env));
  
end:
  
  CORE_EXIT("macro");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("lambda");

  REQUIRE(env, args, TAILP(CAR(args)) || SYMBOLP(CAR(args)));
  REQUIRE(env, args, TAILP(CDR(args)));

  // symbols used in params must not be '*special*':
  if (SYMBOLP(CAR(args))) {
      REQUIRE(env, args, ! SPECIAL_SYMP(CAR(args)), "special symbols cannot be used as params");
  }
  else {
    FOR_EACH (param, CAR(args)) {
      REQUIRE(env, args, SYMBOLP(param));
      REQUIRE(env, args, ! SPECIAL_SYMP(param), "special symbols cannot be used as params");
    }
  }

  RETURN(NEW_LAMBDA(CAR(args), CDR(args), env));
  
end:
  
  CORE_EXIT("lambda");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("params");

  REQUIRE(env, args, MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_PARAMS(CAR(args)));
  
end:
  
  CORE_EXIT("params");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("body");

  REQUIRE(env, args, MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_BODY(CAR(args)));
  
end:
  
  CORE_EXIT("body");
}

