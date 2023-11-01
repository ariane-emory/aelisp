#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aset(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("aset");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * alist = CADR(args);
  ae_obj_t * value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, TAILP(alist), "ALIST must be a list");

  ASET(alist, key, value);
  
  CORE_RETURN("aset", value);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aget(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("aget");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * alist = CADR(args);

  REQUIRE(env, args, TAILP(alist), "ALIST must be a list");

  CORE_RETURN("aget", AGET(alist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _ahas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_ahas(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("ahas");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * alist = CADR(args);

  REQUIRE(env, args, TAILP(alist), "ALIST must be a list");
    
  CORE_RETURN("ahas", TRUTH(AHAS(alist, key)));
}

