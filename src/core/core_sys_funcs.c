#include <unistd.h>
#include <time.h>

#include "core_includes.h"
#include "common.h"
#include "env.h"
#include "time_funcs.h"
#include "pool.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _now
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_allocated(__attribute__((unused)) ae_obj_t * const env,
                       __attribute__((unused)) ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("allocated");
  CORE_RETURN("allocated", NEW_INT(pool_allocated));
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
    RETURN(NEW_ERROR("failed to open file"));
  
  ae_obj_t * new_program = load_file(STR_VAL(CAR(args)), &failed_to_open);

  ret = RETURN_IF_ERRORP(EVAL(env, new_program));

end:
  
  CORE_RETURN("load-file", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// find_file
////////////////////////////////////////////////////////////////////////////////////////////////////

char * find_file(ae_obj_t * const env,
                 bool add_extension,
                 const char * const name) {
  bool load_path_found = false;
  ae_obj_t * const load_path = ENV_GET(env, SYM("*load-path*"), &load_path_found);

  assert(load_path_found);
  assert(load_path);
  
  FOR_EACH(dir, load_path) {
    char * const possible_path = add_extension
      ? free_list_malloc(strlen(STR_VAL(dir)) + strlen(name) + 7)
      : free_list_malloc(strlen(STR_VAL(dir)) + strlen(name) + 2);
    
    sprintf(possible_path,
            add_extension ? "%s/%s.lisp" : "%s/%s",
            STR_VAL(dir), name);

    if (access(possible_path, F_OK) != -1)
      return possible_path;
    else
      free_list_free(possible_path);
  }

  return NULL;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// have_feature
////////////////////////////////////////////////////////////////////////////////////////////////////

static bool have_feature(ae_obj_t * const env, ae_obj_t * const sym) {
  assert(sym);
  assert(SYMBOLP(sym) && (! KEYWORDP(sym)));
  assert(env);
  assert(ENVP(env));

  bool features_found = false;
  ae_obj_t * const features = ENV_GET(env, SYM("*features*"), &features_found);

  assert(features_found);
  assert(features);
  assert(TAILP(features));
  
  FOR_EACH(feature, features)
    if (EQL(feature, sym))
      return true;
  
  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// load_or_require
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum {
  LOAD,
  REQUIRE,
  REREQUIRE
} load_or_require_mode_t;

static ae_obj_t * load_or_require(load_or_require_mode_t mode,
                                  ae_obj_t * const env,
                                  ae_obj_t * const args,
                                  __attribute__((unused)) int args_length) {
  CORE_BEGIN("load_or_require");

  ae_obj_t * const load_target = CAR(args);

  
  if ((mode == REQUIRE) && have_feature(env, load_target))
    RETURN(load_target);

  // Args will have already been checked by the caller, so don't bother doing this:
  // REQUIRE(env, args, (SYMBOLP(load_target) || ! KEYWORDP(load_target)) || STRINGP(load_target));

  const char * const load_target_string = SYMBOLP(load_target) ? SYM_VAL(load_target) : STR_VAL(load_target);
  char * file_path                      = find_file(env,
                                                    mode != LOAD, 
                                                    load_target_string);
    
  bool no_error = (args_length == 2) && ! NILP(CADR(args));

  if (! file_path)
    RETURN(no_error ? NIL : NEW_ERROR("could not find file for '%s", load_target_string));
  
  ae_obj_t * const new_program = RETURN_IF_ERRORP(load_file(file_path, NULL));

  free_list_free(file_path);

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

  if ((mode == REQUIRE || mode == REREQUIRE) && ! have_feature(env, load_target))
    RETURN(NEW_ERROR("required file did not provide '%s", load_target_string));

  bool sprograms_found = false;
  ae_obj_t * sprograms = ENV_GET(env, SYM("*program*"), &sprograms_found);
  assert(sprograms_found);
  assert(sprograms);
  assert(TAILP(sprograms));
  // sprograms = KSET(sprograms, load_target, new_program); // temporarily disabled
  ENV_SET_G(env, SYM("*program*"), sprograms);
  
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

  REQUIRE(env, args, STRINGP(CAR(args)));

  CORE_RETURN("load", load_or_require(LOAD, env, args, args_length));
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

  CORE_RETURN("require", load_or_require(REQUIRE, env, args, args_length));
}

   
////////////////////////////////////////////////////////////////////////////////////////////////////
// _requireb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_requireb(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("requireb");

  REQUIRE(env, args,
          SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  CORE_RETURN("requireb", load_or_require(REREQUIRE, env, args, args_length));
}

   
