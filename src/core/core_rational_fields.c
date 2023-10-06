#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _numer
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_numer(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("numer");

  REQUIRE(env, args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  CORE_RETURN("numer", NEW_INT(NUMER_VAL(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _denom
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_denom(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("denom");

  REQUIRE(env, args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  CORE_RETURN("denom", NEW_INT((RATIONALP(CAR(args)))
                               ? DENOM_VAL(CAR(args))
                               : 1));
}

