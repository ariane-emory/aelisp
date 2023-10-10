#include "core_includes.h"


////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("macro");

  REQUIRE(env, args, TAILP(CAR(args)));
  REQUIRE(env, args, TAILP(CDR(args)));

  CORE_RETURN("macro", NEW_MACRO(CAR(args), CDR(args), env));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("lambda");

  REQUIRE(env, args, TAILP(CAR(args))
#ifndef AE_NO_SINGLE_SYM_PARAMS
          || SYMBOLP(CAR(args))
#endif
          );
  REQUIRE(env, args, TAILP(CDR(args)));

  CORE_RETURN("lambda", NEW_LAMBDA(CAR(args), CDR(args), env));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("params");

  REQUIRE(env, args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  CORE_RETURN("params", FUN_PARAMS(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("body");

  REQUIRE(env, args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  CORE_RETURN("body", FUN_BODY(CAR(args)));
}

