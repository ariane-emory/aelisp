#include <stdbool.h>

#include "core_includes.h"

extern bool log_eval;
extern bool log_core;

////////////////////////////////////////////////////////////////////////////////////////////////////
// _l_eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_l_eval(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_BEGIN("l_eval");

  (void)args;
  (void)env;
  (void)args_length;
  
  bool old_value =  log_eval;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_eval =TRUEP(CAR(args));
  }

  CORE_RETURN("l_eval", old_value ? TRUE : NIL);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _l_core
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_l_core(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_BEGIN("l_core");

  (void)args;
  (void)env;
  (void)args_length;
  
  bool old_value =  log_core;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_core =TRUEP(CAR(args));
  }

  CORE_RETURN("l_core", old_value ? TRUE : NIL);
}

