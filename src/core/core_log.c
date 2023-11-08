#include <stdbool.h>

#include "core_includes.h"
#include "list.h"
#include "common.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_eval(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_ENTER("l_eval");
  
  bool old_value = log_eval;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_eval = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'eval' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'eval' LOGGING ON!\n");
  }

  RETURN(TRUTH(old_value));
  
end:
  
  CORE_EXIT("l_eval");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_core
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_core(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_ENTER("l_core");

  bool old_value = log_core;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_core = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'core' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'core' LOGGING ON!\n");
  }

  RETURN(TRUTH(old_value));

end:
  
  CORE_EXIT("l_core");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_macro(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_ENTER("l_macro");

  bool old_value = log_macro;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_macro = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'macro' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'macro' LOGGING ON!\n");
  }
  
  RETURN(TRUTH(old_value));

end:
  
  CORE_EXIT("l_macro");
}
