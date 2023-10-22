#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("car");

  if (! TAILP(CAR(args))) { 
    LOG(CAR(args), "not TAILP:");
    REQUIRE(env, args, TAILP(CAR(args)));
  }

  CORE_RETURN("car", NILP(CAR(args))
              ? NIL // car of nil is nil.
              : CAAR(args));
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("cdr");

  if (! TAILP(CAR(args))) {
    LOG(CAR(args), "not TAILP:");
    REQUIRE(env, args, TAILP(CAR(args)));
  }

  CORE_RETURN("cdr", NILP(CAR(args))
              ? NIL // cdr of nil is nil.
              : CDAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplaca
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplaca(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("rplaca");

  REQUIRE(env, args, CONSP(CAR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objests cannot be mutated");
  
  CAAR(args) = CADR(args);

  CORE_RETURN("rplaca", CADR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplacd
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplacd(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("rplacd");

  REQUIRE(env, args, CONSP(CAR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objests cannot be mutated");
  
  CDAR(args) = CADR(args);

  CORE_RETURN("rplacd", CADR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(
  __attribute__((unused)) ae_obj_t * const env,
  ae_obj_t * const args,
  __attribute__((unused)) int args_length) {
  CORE_BEGIN("cons");

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);

  CORE_RETURN("cons", NEW_CONS(head, tail));
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("length");

  // REQUIRE(env, args, PROPERP(CAR(args)), "core length only works on proper lists");

  CORE_RETURN("length", NEW_INT(LENGTH(CAR(args))));
}
