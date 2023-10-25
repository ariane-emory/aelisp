#include <string.h>

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
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  
  CAAR(args) = CADR(args);

  CORE_RETURN("rplaca", CADR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplacd
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplacd(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("rplacd");

  REQUIRE(env, args, CONSP(CAR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  
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

  REQUIRE(env, args, CONSP(CAR(args)) || STRINGP(CAR(args)), "core length only works on lists and strings");
  
  ret = CONSP(CAR(args))
    ? NEW_INT(LENGTH(CAR(args)))
    : NEW_INT(strlen(STR_VAL(CAR(args))));
  
  CORE_RETURN("length", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_push(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("push");

  REQUIRE(env, args, TAILP(CADR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CADR(args)), "read-only objects cannot be mutated");
  

  CORE_RETURN("push", PUSH(CAR(args), CADR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_push_back(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("push_back");

  REQUIRE(env, args, TAILP(CAR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  

  CORE_RETURN("push_back", PUSH_BACK(CAR(args), CADR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pop(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pop");

  REQUIRE(env, args, CONSP(CAR(args)));
  REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  

  CORE_RETURN("pop", POP(CAR(args)));
}
