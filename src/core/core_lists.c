#include <string.h>

#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(car) {
  if (! TAILP(CAR(args))) { 
    LOG(CAR(args), "not TAILP:");
    REQUIRE(TAILP(CAR(args)));
  }

  RETURN(NILP(CAR(args))
         ? NIL // car of nil is nil.
         : CAAR(args));
  
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(cdr) {
  if (! TAILP(CAR(args))) {
    LOG(CAR(args), "not TAILP:");
    REQUIRE(TAILP(CAR(args)));
  }

  RETURN(NILP(CAR(args))
         ? NIL // cdr of nil is nil.
         : CDAR(args));
  
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplaca
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(rplaca) {
  REQUIRE(CONSP(CAR(args)));
  CAAR(args) = CADR(args);
  RETURN(CADR(args));
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplacd
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(rplacd) {
  REQUIRE(CONSP(CAR(args)));
  CDAR(args) = CADR(args);
  RETURN(CADR(args));
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(cons) {
  ae_obj_t * const head = CAR(args);
  ae_obj_t * const tail = CADR(args);

  RETURN(NEW_CONS(head, tail));  
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(length) {
  REQUIRE(TAILP(CAR(args)) || STRINGP(CAR(args)), "core length only works on lists and strings");

  if (NILP(CAR(args)))
    RETURN(NEW_INT(0));
  
  RETURN(CONSP(CAR(args))
         ? NEW_INT(LENGTH(CAR(args)))
         : NEW_INT(strlen(STR_VAL(CAR(args)))));
  
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _pop
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(pop) {
  ae_obj_t * const sym  = CAR(args);

  REQUIRE(SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "pop only works on bound and settable symbols");

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  if (NILP(lst))
    RETURN(NIL);
  
  REQUIRE(CONSP(lst));

  ae_obj_t * const tail = CDR(lst);
  
  ret = CAR(lst);

  ENV_SET(env, sym, tail);

  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _push
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(push) {
  ae_obj_t * const sym = CADR(args);

  REQUIRE(SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "push only works on bound and settable symbols");

  ae_obj_t * const lst = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(TAILP(lst));

  ae_obj_t * const val = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ret                  = CONS(val, lst);

  ENV_SET(env, sym, ret);
  
  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
