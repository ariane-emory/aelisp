#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premoveb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premoveb(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("premove!");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PREMOVE_MUTATING(plist, key);
  
  CORE_RETURN("premove!", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premove
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premove(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("premove");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PREMOVE_NONMUTATING(plist, key);
  
  CORE_RETURN("pset", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _psetb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_psetb(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("pset!");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PSET_MUTATING(plist, key, value);
  
  CORE_RETURN("pset!", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pset(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pset");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PSET_NONMUTATING(plist, key, value);
  
  CORE_RETURN("pset", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pget(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pget");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  CORE_RETURN("pget", PGET(plist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_phas(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("phas");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");
    
  CORE_RETURN("phas", TRUTH(PHAS(plist, key)));
}

