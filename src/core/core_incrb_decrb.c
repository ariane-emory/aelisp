#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _incrb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_incrb(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("incr!");

  ae_obj_t * const sym = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "incr! only works on bound and settable symbols");
  
  ae_obj_t * const integer = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, INTEGERP(integer));

  ret = NEW_INT(1 + INT_VAL(integer));

  ENV_SET(env, sym, ret);
  
end:
  
  CORE_RETURN("incr!", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _decrb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_decrb(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("decr!");

  ae_obj_t * const sym = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "decr! only works on bound and settable symbols");
  
  ae_obj_t * const integer = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, INTEGERP(integer));

  ret = NEW_INT(1 + INT_VAL(integer));

  ENV_SET(env, sym, ret);
  
end:
  
  CORE_RETURN("decr!", ret);
}
