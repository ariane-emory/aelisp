#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rational
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rational(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("rational");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  REQUIRE(env, args, INTEGERP(CADR(args)));

  RETURN(NEW_RATIONAL(INT_VAL(CAR(args)), INT_VAL(CADR(args))));

  CORE_END("rational");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _numer
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_numer(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("numer");
  REQUIRE(env, args, RATIONALP(CAR(args)) || INTEGERP(CAR(args)));

  RETURN(INTEGERP(CAR(args))
         ? CAR(args)
         : NEW_INT(NUMER_VAL(CAR(args))));
  
  CORE_END("numer");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _denom
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_denom(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("denom");
  REQUIRE(env, args, RATIONALP(CAR(args)) || INTEGERP(CAR(args)));

  RETURN(NEW_INT((RATIONALP(CAR(args)))
                 ? DENOM_VAL(CAR(args))
                 : 1));
  CORE_END("denom");
}

