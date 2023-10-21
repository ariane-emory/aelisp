#include "core_includes.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("env");

  if (args_length == 1) {
    REQUIRE(env, args, (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

    CORE_RETURN("env", ENVP(CAR(args))
                ? ENV_PARENT(CAR(args))
                : FUN_ENV(CAR(args)));
  }

  CORE_RETURN("env", env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _syms
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_syms(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("syms");

  if (args_length == 1) {
    REQUIRE(env, args, ENVP(CAR(args)));
    CORE_RETURN("syms", ENV_SYMS(CAR(args)));
  }

  CORE_RETURN("syms", ENV_SYMS(env));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _vals
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_vals(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("vals");

  if (args_length == 1) {
    REQUIRE(env, args, ENVP(CAR(args)));
    CORE_RETURN("vals", ENV_VALS(CAR(args)));
  }

  CORE_RETURN("vals", ENV_VALS(env));
}

