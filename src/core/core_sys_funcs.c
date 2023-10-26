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

ae_obj_t * ae_core_load_file(ae_obj_t * const env,
                             ae_obj_t * const args,
                             __attribute__((unused)) int args_length) {
  CORE_BEGIN("load-file");
  
  REQUIRE(env, args, STRINGP(CAR(args)));

  bool failed_to_open = false;

  if (failed_to_open)
    RETURN(NEW_ERROR("failed to open file", NIL));
  
  ae_obj_t * new_program = load_file(STR_VAL(CAR(args)), &failed_to_open);

  ret = RETURN_IF_ERRORP(EVAL(env, new_program));

end:
  
  CORE_RETURN("load-file", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _load_or_require
////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_obj_t * ae_core_load_or_require(bool check_feature,
                                          ae_obj_t * const env,
                                          ae_obj_t * const args,
                                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("load_or_require");

  ae_obj_t * const new_feature  = CAR(args);

  REQUIRE(env, args,
          SYMBOLP(new_feature)      &&
          (! KEYWORDP(new_feature)) &&
          (! NILP(new_feature))     &&
          (! TRUEP(new_feature)));

  char * file_found = NULL;
  
  FOR_EACH(dir, ENV_GET(env, SYM("*load-path*"))) {
    // PR("\nLooking in %s...\n", STR_VAL(dir));

    char * const possible_path = free_list_malloc(strlen(STR_VAL(dir)) + strlen(SYM_VAL(new_feature)) + 7);
    
    sprintf(possible_path, "%s/%s.lisp", STR_VAL(dir), SYM_VAL(new_feature));

    // PR("Trying %s... ", possible_path);
    
    if (access(possible_path, F_OK) != -1) {
      PR("found it.\n");
      file_found = possible_path;
      
      break;
    }
    else {
      PR("not found.\n");
      free_list_free(possible_path);
    }
  }
  
  if (!file_found) {
    char * const tmp = free_list_malloc(256);
    snprintf(tmp, 256, "could not find file for '%s", SYM_VAL(new_feature));
    char * const err_msg = free_list_malloc(strlen(tmp) + 1);
    strcpy(err_msg, tmp);
    free_list_free(tmp);

    RETURN_IF_ERRORP(NEW_ERROR(err_msg, NIL));
  }

  ae_obj_t * const new_program = RETURN_IF_ERRORP(load_file(file_found, NULL));
  const bool old_log_macro     = log_macro;
  const bool old_log_core      = log_core;
  const bool old_log_eval      = log_eval;
  log_macro                    = false;
  log_core                     = false;
  log_eval                     = false;
  ret                          = RETURN_IF_ERRORP(EVAL(env, new_program));
  log_macro                    = old_log_macro;
  log_core                     = old_log_core;
  log_eval                     = old_log_eval;

  if (check_feature) {
    bool feature_found           = false;
    ae_obj_t * const features    = ENV_GET(env, SYM("*features*"));
  
    FOR_EACH(feature, features) {
      if (EQL(feature, new_feature)) {
        feature_found = true;
      
        break;
      }
    }

    if (!feature_found) {
      char * const tmp = free_list_malloc(256);
      snprintf(tmp, 256, "required file did not provide '%s", SYM_VAL(new_feature));
      char * const err_msg = free_list_malloc(strlen(tmp) + 1);
      strcpy(err_msg, tmp);
      free_list_free(tmp);

      RETURN_IF_ERRORP(NEW_ERROR(err_msg, NIL));
    }
  }
  
end:
  
  CORE_RETURN("load_or_require", ret);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _load
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_load(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("load");
  CORE_RETURN("load", ae_core_load_or_require(false, env, args, args_length));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _require
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_require(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("require");
  CORE_RETURN("require", ae_core_load_or_require(true, env, args, args_length));
}
   
