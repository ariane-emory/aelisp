#include <string.h>

#include "core_includes.h"

#include "env.h"

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

  ae_obj_t * const head = CAR(args);
  ae_obj_t * const tail = CADR(args);

  CORE_RETURN("cons", NEW_CONS(head, tail));
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("length");

  REQUIRE(env, args, TAILP(CAR(args)) || STRINGP(CAR(args)), "core length only works on lists and strings");

  if (NILP(CAR(args)))
    RETURN(NEW_INT(0));
  
  ret = CONSP(CAR(args))
    ? NEW_INT(LENGTH(CAR(args)))
    : NEW_INT(strlen(STR_VAL(CAR(args))));
  
end:

  CORE_RETURN("length", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _popb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_popb(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pop!");

  ae_obj_t * const sym  = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "popb! only works on bound and settable symbols");

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, CONSP(lst));

  ae_obj_t * const tail = CDR(lst);
  
  ret = CAR(lst);

  ENV_SET(env, sym, tail);

end:
  
  CORE_RETURN("pop!", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pushb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pushb(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("push!");

  ae_obj_t * const sym = CADR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "push! only works on bound and settable symbols");

  ae_obj_t * const lst = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, TAILP(lst));

  ae_obj_t * const val = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ret                  = CONS(val, lst);

  ENV_SET(env, sym, ret);
  
end:
  
  CORE_RETURN("push!", ret);
}
