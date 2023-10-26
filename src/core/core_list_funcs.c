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
// _pop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pop(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pop");

  ae_obj_t * const sym = CAR(args);

  REQUIRE(env, args, SYMBOLP(sym));

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, CONSP(lst));
  
  ret = CAR(lst);
  
  ae_obj_t * const setq_args = CONS(sym, CONS(CONS(SYM("quote"), CONS(CDR(lst), NIL)), NIL));

  LOG(setq_args, "setq_args");
  
  RETURN_IF_ERRORP(ae_core_setq(env, setq_args, 2));

end:
  
  CORE_RETURN("pop", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_push(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("push");

  ae_obj_t * const sym = CADR(args);

  REQUIRE(env, args, SYMBOLP(sym));

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, TAILP(lst));

  ae_obj_t * const val = CAR(args);

  ae_obj_t * const new_list = CONS(val, lst);
  
  ret = new_list;
  
  ae_obj_t * const setq_args = CONS(sym, CONS(CONS(SYM("quote"), CONS(new_list, NIL)), NIL));

  LOG(setq_args, "setq_args");
  
  RETURN_IF_ERRORP(ae_core_setq(env, setq_args, 2));

end:
  
  CORE_RETURN("push", ret);
}
