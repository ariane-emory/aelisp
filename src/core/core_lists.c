#include <string.h>

#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("car");

  if (! TAILP(CAR(args))) { 
    LOG(CAR(args), "not TAILP:");
    REQUIRE(env, args, TAILP(CAR(args)));
  }

  RETURN(NILP(CAR(args))
         ? NIL // car of nil is nil.
         : CAAR(args));
  
end:
  
  CORE_EXIT("car");
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("cdr");

  if (! TAILP(CAR(args))) {
    LOG(CAR(args), "not TAILP:");
    REQUIRE(env, args, TAILP(CAR(args)));
  }

  RETURN(NILP(CAR(args))
         ? NIL // cdr of nil is nil.
         : CDAR(args));
  
end:
  
  CORE_EXIT("cdr");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplaca
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplaca(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("rplaca");

  REQUIRE(env, args, CONSP(CAR(args)));
  // REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  
  CAAR(args) = CADR(args);

  RETURN(CADR(args));
  
end:
  
  CORE_EXIT("rplaca");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplacd
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplacd(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("rplacd");

  REQUIRE(env, args, CONSP(CAR(args)));
  // REQUIRE(env, args, ! HAS_PROP("read-only", CAR(args)), "read-only objects cannot be mutated");
  
  CDAR(args) = CADR(args);

  RETURN(CADR(args));
  
end:
  
  CORE_EXIT("rplacd");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(
  __attribute__((unused)) ae_obj_t * const env,
  ae_obj_t * const args,
  __attribute__((unused)) int args_length) {
  CORE_ENTER("cons");

  ae_obj_t * const head = CAR(args);
  ae_obj_t * const tail = CADR(args);

  RETURN(NEW_CONS(head, tail));
  
end:
  
  CORE_EXIT("cons");
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("length");

  REQUIRE(env, args, TAILP(CAR(args)) || STRINGP(CAR(args)), "core length only works on lists and strings");

  if (NILP(CAR(args)))
    RETURN(NEW_INT(0));
  
  RETURN(CONSP(CAR(args))
         ? NEW_INT(LENGTH(CAR(args)))
         : NEW_INT(strlen(STR_VAL(CAR(args)))));
  
end:

  CORE_EXIT("length");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pop(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_ENTER("pop");

  ae_obj_t * const sym  = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "pop only works on bound and settable symbols");

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  if (NILP(lst))
    RETURN(NIL);
  
  REQUIRE(env, args, CONSP(lst));

  ae_obj_t * const tail = CDR(lst);
  
  ret = CAR(lst);

  ENV_SET(env, sym, tail);

end:
  
  CORE_EXIT("pop");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_push(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("push");

  ae_obj_t * const sym = CADR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "push only works on bound and settable symbols");

  ae_obj_t * const lst = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, TAILP(lst));

  ae_obj_t * const val = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ret                  = CONS(val, lst);

  ENV_SET(env, sym, ret);
  
end:
  
  CORE_EXIT("push");
}
