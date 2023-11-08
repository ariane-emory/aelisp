#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _incrb
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(incr) {
  ae_obj_t * const sym = CAR(args);

  REQUIRE(SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "incr only works on bound and settable symbols");
  
  ae_obj_t * const integer        = RETURN_IF_ERRORP(EVAL(env, sym));
  ae_obj_t * const addend_expr    = NILP(CDR(args)) ? NIL : CADR(args);
  ae_obj_t * const addend_integer = RETURN_IF_ERRORP(EVAL(env, addend_expr));

  REQUIRE(INTEGERP(integer));
  REQUIRE(NILP(addend_integer) || INTEGERP(addend_integer));

  ret = NEW_INT(INT_VAL(integer) + (NILP(addend_integer) ? 1 : INT_VAL(addend_integer)));

  ENV_SET(env, sym, ret);
  
  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _decrb
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(decr) {
  ae_obj_t * const sym = CAR(args);

  REQUIRE(SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "decr only works on bound and settable symbols");
  
  ae_obj_t * const integer            = RETURN_IF_ERRORP(EVAL(env, sym));
  ae_obj_t * const subtrahend_expr    = NILP(CDR(args)) ? NIL : CADR(args);
  ae_obj_t * const subtrahend_integer = RETURN_IF_ERRORP(EVAL(env, subtrahend_expr));

  REQUIRE(INTEGERP(integer));
  REQUIRE(NILP(subtrahend_integer) || INTEGERP(subtrahend_integer));

  ret = NEW_INT(INT_VAL(integer) - (NILP(subtrahend_integer) ? 1 : INT_VAL(subtrahend_integer)));

  ENV_SET(env, sym, ret);
  
  END_DEF_CORE_FUN;
}
