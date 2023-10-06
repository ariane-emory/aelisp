#include "core_includes.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("env");

  int len = LENGTH(args);

  REQUIRE(env, args, len <= 1, "env requires 0 or 1 args");

  if (len == 1) {
    REQUIRE(env, args, (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

    CORE_RETURN("env", ENVP(CAR(args))
                ? ENV_PARENT(CAR(args))
                : FUN_ENV(CAR(args)));
  }

  CORE_RETURN("env", env);
}

