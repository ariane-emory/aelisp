#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pset(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("pset");

  int len = LENGTH(args);

  REQUIRE(env, args, len >= 2, "pset requires at least 2 args");
  REQUIRE(env, args, len <= 3, "pset requires 2 or 3 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);
  ae_obj_t * value = CADDR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("pset", PSET(plist, key, value));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pget(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("pget");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "pget requires 2 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("pget", PGET(plist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_phas(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("phas");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "pget requires 2 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("phas", TRUTH(PHAS(plist, key)));
}

