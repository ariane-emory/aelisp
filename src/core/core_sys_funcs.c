#include <unistd.h>
#include <time.h>

#include "core_includes.h"
#include "common.h"
#include "env.h"
#include "time_funcs.h"

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
  
  ae_obj_t * new_program = load_file(STR_VAL(CAR(args)), &failed_to_open);
  ae_obj_t * ret         = EVAL(env, new_program);

  CORE_RETURN("load", failed_to_open ? NEW_ERROR("failed to open file", NIL) : ret);
}
