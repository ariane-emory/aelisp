#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premoveb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premoveb(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("premove!");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PREMOVE_MUTATING(plist, key);
  
  CORE_RETURN("premove!", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premove
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premove(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("premove");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  plist = PREMOVE_NONMUTATING(plist, key);
  
  CORE_RETURN("pset", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _psetb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_psetb(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("pset!");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);
  ae_obj_t * value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PSET_MUTATING(plist, key, value);
  
  CORE_RETURN("pset!", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pset(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pset");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);
  ae_obj_t * value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  plist = PSET_NONMUTATING(plist, key, value);
  
  CORE_RETURN("pset", plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pget(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pget");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  CORE_RETURN("pget", PGET(plist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_phas(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("phas");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * plist = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");
    
  CORE_RETURN("phas", TRUTH(PHAS(plist, key)));
}

