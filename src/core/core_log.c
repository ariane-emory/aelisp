#include <stdbool.h>

#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_acc_log_eval(ae_obj_t * const env,
                                ae_obj_t * const args,
                                int args_length) {
  CORE_BEGIN("log_eval");

  (void)args;
  (void)env;
  (void)args_length;
  
  bool old_value = false; // log_eval;

  /* (void)args;   */

  /* if (args_length == 1) { */
  /* /\*   REQUIRE(env, args, (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args)))); *\/ */

  /* /\*   CORE_RETURN("log_eval", ENVP(CAR(args)) *\/ */
  /* /\*               ? ENV_PARENT(CAR(args)) *\/ */
  /* /\*               : FUN_ENV(CAR(args))); *\/ */
  /* } */

  CORE_RETURN("log_eval", old_value ? TRUE : NIL);
}

