#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premoveb
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(premoveb) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PREMOVE_MUTATING(plist, key);

  ret = plist;
  
  END_DEF_CORE_FUN(premove!);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _premove
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(premove) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PREMOVE_NONMUTATING(plist, key);

  END_DEF_CORE_FUN(pset);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _psetb
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(psetb) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, CONSP(plist), "PLIST must be a non-empty list");

  PSET_MUTATING(plist, key, value);

  ret = plist;
  
  END_DEF_CORE_FUN(pset!);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(pset) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);
  ae_obj_t * const value = CADDR(args); // this could be unsave if value is NIL, maybe.

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PSET_NONMUTATING(plist, key, value);

  END_DEF_CORE_FUN(pset);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(pget) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = PGET(plist, key);
  
  END_DEF_CORE_FUN(pget);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(phas) {
  ae_obj_t * const plist = CAR(args);
  ae_obj_t * const key   = CADR(args);

  REQUIRE(env, args, TAILP(plist), "PLIST must be a list");

  ret = TRUTH(PHAS(plist, key));
  
  END_DEF_CORE_FUN(phas);
}

