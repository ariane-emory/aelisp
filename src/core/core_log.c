#include <stdbool.h>

#include "core_includes.h"
#include "list.h"

extern bool log_eval;
extern bool log_core;

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_all
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_all(ae_obj_t * const env,
                         ae_obj_t * const args,
                         int args_length) {
  CORE_BEGIN("l_all");

  bool old_log_eval_value = log_eval;
  bool old_log_core_value = log_core;

  if (args_length == 1) {
    REQUIRE(env, args, (SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))))
    || (PROPERP(CAR(args)) &&
        (LENGTH(CAR(args)) == 2) &&
        (NILP(CAAR(args)) || TRUEP(CAAR(args))) &&
        (NILP(CADAR(args)) || TRUEP(CADAR(args)))));

    if (SYMBOLP(CAR(args))) {
      SLOG("CASE 1");

      bool set_val = TRUEP(CAR(args));
      
      log_eval = set_val;
      log_core = set_val;
    }
    else {
      SLOG("CASE 2");
      OLOG(args);
      OLOG(CAAR(args));
      OLOG(CADAR(args));
      
      log_eval = TRUEP(CAAR(args));
      log_core = TRUEP(CADAR(args));
    }

    if      ((! log_eval) && (  old_log_eval_value))
      SLOG("TURNING 'eval' LOGGING OFF!\n");
    else if ((  log_eval) && (! old_log_eval_value))
      SLOG("TURNING 'eval' LOGGING ON!\n");

    if      ((! log_core) && (  old_log_core_value))
      SLOG("TURNING 'core' LOGGING OFF!\n");
    else if ((  log_core) && (! old_log_core_value))
      SLOG("TURNING 'core' LOGGING ON!\n");
  }

  CORE_RETURN("l_all", CONS(old_log_eval_value ? TRUE : NIL,
                            CONS(old_log_core_value ? TRUE : NIL,
                                 NIL)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_eval(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_BEGIN("l_eval");
  
  bool old_value = log_eval;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_eval = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'eval' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'eval' LOGGING ON!\n");
  }

  CORE_RETURN("l_eval", old_value ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_core
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_core(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_BEGIN("l_core");

  bool old_value = log_core;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_core = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'core' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'core' LOGGING ON!\n");
  }

  CORE_RETURN("l_core", old_value ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _log_macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_log_macro(ae_obj_t * const env,
                          ae_obj_t * const args,
                          int args_length) {
  CORE_BEGIN("l_macro");

  bool old_value = log_macro;

  if (args_length == 1) {
    REQUIRE(env, args, SYMBOLP(CAR(args)) && (NILP(CAR(args)) || TRUEP(CAR(args))));

    log_macro = TRUEP(CAR(args));

    if      (NILP(CAR(args)) && old_value)
      SLOG("TURNING 'macro' LOGGING OFF!\n");
    else if (TRUEP(CAR(args)) && !old_value)
      SLOG("TURNING 'macro' LOGGING ON!\n");
  }

  CORE_RETURN("l_macro", old_value ? TRUE : NIL);
}
