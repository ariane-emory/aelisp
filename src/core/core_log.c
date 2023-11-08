#include <stdbool.h>

#include "core_includes.h"
#include "list.h"
#include "common.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_eval
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(log_eval) {
  bool old_value = log_eval;

  if (args_length == 1) {
    REQUIRE(SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_eval = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'eval' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'eval' LOGGING ON!\n");
  }

  RETURN(TRUTH(old_value));
  
  END_DEF_CORE_FUN(l_eval);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_core
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(log_core) {
  bool old_value = log_core;

  if (args_length == 1) {
    REQUIRE(SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_core = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'core' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'core' LOGGING ON!\n");
  }

  RETURN(TRUTH(old_value));

  END_DEF_CORE_FUN(l_core);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_macro
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(log_macro) {
  bool old_value = log_macro;

  if (args_length == 1) {
    REQUIRE(SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_macro = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'macro' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'macro' LOGGING ON!\n");
  }
  
  RETURN(TRUTH(old_value));

  END_DEF_CORE_FUN(l_macro);
}
