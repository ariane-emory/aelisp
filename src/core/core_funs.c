#include "core_includes.h"


////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("macro");

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
  
  CORE_END("macro");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("lambda");

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
  
  CORE_END("lambda");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("params");

  REQUIRE(env, args, MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_PARAMS(CAR(args)));
  
  CORE_END("params");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("body");

  REQUIRE(env, args, MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_BODY(CAR(args)));
  
  CORE_END("body");
}

