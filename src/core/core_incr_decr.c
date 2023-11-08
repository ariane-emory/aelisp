#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _incrb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_incr(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("incr");

  ae_obj_t * const sym = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "incr only works on bound and settable symbols");
  
  ae_obj_t * const integer        = RETURN_IF_ERRORP(EVAL(env, sym));
  ae_obj_t * const addend_expr    = NILP(CDR(args)) ? NIL : CADR(args);
  ae_obj_t * const addend_integer = RETURN_IF_ERRORP(EVAL(env, addend_expr));

  REQUIRE(env, args, INTEGERP(integer));
  REQUIRE(env, args, NILP(addend_integer) || INTEGERP(addend_integer));

  ret = NEW_INT(INT_VAL(integer) + (NILP(addend_integer) ? 1 : INT_VAL(addend_integer)));

  ENV_SET(env, sym, ret);
  
  CORE_END("incr");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _decrb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_decr(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("decr");

  ae_obj_t * const sym = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "decr only works on bound and settable symbols");
  
  ae_obj_t * const integer            = RETURN_IF_ERRORP(EVAL(env, sym));
  ae_obj_t * const subtrahend_expr    = NILP(CDR(args)) ? NIL : CADR(args);
  ae_obj_t * const subtrahend_integer = RETURN_IF_ERRORP(EVAL(env, subtrahend_expr));

  REQUIRE(env, args, INTEGERP(integer));
  REQUIRE(env, args, NILP(subtrahend_integer) || INTEGERP(subtrahend_integer));

  ret = NEW_INT(INT_VAL(integer) - (NILP(subtrahend_integer) ? 1 : INT_VAL(subtrahend_integer)));

  ENV_SET(env, sym, ret);
  
  CORE_END("decr");
}
