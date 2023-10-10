#include <unistd.h>
#include <time.h>
#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _time
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_time(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("time");
  REQUIRE(env, args, NILP(args), "time takes no arguments");

  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  long long elapsed = ((long long) ts.tv_sec * 1000000LL + ts.tv_nsec / 1000LL);

  CORE_RETURN("time", NEW_INT(elapsed));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _msleep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_sleep(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("msleep");

  REQUIRE(env, args, (LENGTH(args) == 1) && INTEGERP(CAR(args)));

  int ms = INT_VAL(CAR(args));

  usleep(ms * 1000);

  CORE_RETURN("msleep", CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_exit(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("exit");

  REQUIRE(env, args, NILP(CAR(args)) || INTEGERP(CAR(args)));

  exit(INTEGERP(CAR(args)) ? INT_VAL(CAR(args)) : 0);

  CORE_RETURN("exit", CAR(args));
}

