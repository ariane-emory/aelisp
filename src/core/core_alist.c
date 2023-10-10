#include "core_includes.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// _aset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aset(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("aset");

  int len = LENGTH(args);

  REQUIRE(env, args, len >= 2, "aset requires at least 2 args");
  REQUIRE(env, args, len <= 3, "aset requires 2 or 3 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);
  ae_obj_t * value = CADDR(args);

  // REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("aset", ASET(alist, key, value));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aget(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("aget");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  // REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("aget", AGET(alist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _ahas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_ahas(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("ahas");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  // REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("ahas", TRUTH(AHAS(alist, key)));
}

