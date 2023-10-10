#include <unistd.h>
#include <time.h>
#include "core_includes.h"

static long long int now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((long long int) ts.tv_sec * 1000000LL + ts.tv_nsec / 1000LL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _time
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_time(__attribute__((unused)) ae_obj_t * const env,
                        __attribute__((unused)) ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("time");
  CORE_RETURN("time", NEW_INT(now()));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_elapsed(__attribute__((unused)) ae_obj_t * const env,
                        __attribute__((unused)) ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("elapsed");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  CORE_RETURN("time", NEW_INT(now() - INT_VAL(CAR(args))));
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
  REQUIRE(env, args, NILP(CAR(args)) || INTEGERP(CAR(args)));
  exit(INTEGERP(CAR(args)) ? INT_VAL(CAR(args)) : 0);
  CORE_RETURN("exit", CAR(args));
}

