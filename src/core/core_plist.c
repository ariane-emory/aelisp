#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premoveb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premoveb(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_ENTER("premove!");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PREMOVE_MUTATING(plist, key);

  ret = plist;
  
  CORE_EXIT("premove!");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premove
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_premove(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_ENTER("premove");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PREMOVE_NONMUTATING(plist, key);

  CORE_EXIT("pset");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _psetb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_psetb(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_ENTER("pset!");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PSET_MUTATING(plist, key, value);

  ret = plist;
  
  CORE_EXIT("pset!");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pset(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("pset");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PSET_NONMUTATING(plist, key, value);

  CORE_EXIT("pset");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pget(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("pget");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PGET(plist, key);
  
  CORE_EXIT("pget");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_phas(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("phas");

  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = TRUTH(PHAS(plist, key));
  
  CORE_EXIT("phas");
}

