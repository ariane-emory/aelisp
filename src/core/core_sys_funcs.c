#include <unistd.h>
#include <time.h>

#include "core_includes.h"
#include "common.h"
#include "env.h"
#include "time_funcs.h"
#include "free_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _program
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_program(__attribute__((unused)) ae_obj_t * const env,
                           __attribute__((unused)) ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("program");
  CORE_RETURN("program", program);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _now
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_now(__attribute__((unused)) ae_obj_t * const env,
                       __attribute__((unused)) ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("now");
  CORE_RETURN("now", NEW_INT(now()));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _now_us
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_now_us(__attribute__((unused)) ae_obj_t * const env,
                          __attribute__((unused)) ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("now_us");
  CORE_RETURN("now_us", NEW_INT(now_us()));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_elapsed(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("elapsed");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  CORE_RETURN("elapsed", NEW_INT(elapsed(INT_VAL(CAR(args)))));
  // CORE_RETURN("elapsed", NEW_INT(now() - INT_VAL(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed_us
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_elapsed_us(ae_obj_t * const env,
                              ae_obj_t * const args,
                              __attribute__((unused)) int args_length) {
  CORE_BEGIN("elapsed_us");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  CORE_RETURN("elapsed", NEW_INT(elapsed_us(INT_VAL(CAR(args)))));
  // CORE_RETURN("elapsed_us", NEW_INT(now_us() - INT_VAL(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _msleep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_sleep(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("msleep");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  usleep(INT_VAL(CAR(args)) * 1000);
  CORE_RETURN("msleep", CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_exit(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("exit");
  
  int exit_code = 0;

  if (args_length == 1) {
    REQUIRE(env, args, INTEGERP(CAR(args)));
    exit_code = INT_VAL(CAR(args));
  }
  
  exit(exit_code);
  
  CORE_RETURN("exit", CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _load
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_load(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("load");
  
  REQUIRE(env, args, STRINGP(CAR(args)));

  bool failed_to_open = false;

  if (failed_to_open)
    RETURN(NEW_ERROR("failed to open file", NIL));
  
  ae_obj_t * new_program = load_file(STR_VAL(CAR(args)), &failed_to_open);

  ret = RETURN_IF_ERRORP(EVAL(env, new_program));

end:
  
  CORE_RETURN("load", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _require
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_require(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("require");
  
  REQUIRE(env, args,
          SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  char * found = NULL;
  
  FOR_EACH(dir, ENV_GET(env, SYM("*load-path*"))) {
    // PR("\nLooking in %s...\n", STR_VAL(dir));

    char * const possible_path = free_list_malloc(strlen(STR_VAL(dir)) + strlen(SYM_VAL(CAR(args))) + 7);
    
    sprintf(possible_path, "%s/%s.lisp", STR_VAL(dir), SYM_VAL(CAR(args)));

    // PR("Trying %s... ", possible_path);
    
    if (access(possible_path, F_OK) != -1) {
      PR("found it.\n");
      found = possible_path;
      
      break;
    }
    else {
      PR("not found.\n");
      free_list_free(possible_path);
    }
  }
  
  if (!found) {
    char * const tmp = free_list_malloc(256);
    snprintf(tmp, 256, "could not find file for '%s", SYM_VAL(CAR(args)));
    char * const err_msg = free_list_malloc(strlen(tmp) + 1);
    strcpy(err_msg, tmp);
    free_list_free(tmp);

    RETURN(NEW_ERROR(err_msg, NIL));
  }

  ae_obj_t * new_program = RETURN_IF_ERRORP(load_file(found, NULL));

  bool old_log_macro     = log_macro;
  bool old_log_core      = log_core;
  bool old_log_eval      = log_eval;
  log_macro              = false;
  log_core               = false;
  log_eval               = false;
  
  ret = RETURN_IF_ERRORP(EVAL(env, new_program));

  log_macro              = old_log_macro;
  log_core               = old_log_core;
  log_eval               = old_log_eval;

end:
  
  CORE_RETURN("require", ret);
}
