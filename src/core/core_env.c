#include "core_includes.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("env");

  if (args_length == 1) {
    REQUIRE(env, args, (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

    if (ENVP(CAR(args))) {
      RETURN(ENV_PARENT(CAR(args)));
    }
    else {
      RETURN(FUN_ENV(CAR(args)));
    }
  }

  RETURN(env);

end:
  
  CORE_RETURN("env");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _syms
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_syms(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("syms");

  if (args_length == 1) {
    REQUIRE(env, args, ENVP(CAR(args)));

    RETURN(ENV_SYMS(CAR(args)));
  }

  RETURN(ENV_SYMS(env));
  
end:
  
  CORE_RETURN("syms");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _vals
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_vals(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("vals");

  if (args_length == 1) {
    REQUIRE(env, args, ENVP(CAR(args)));

    RETURN(ENV_VALS(CAR(args)));
  }

  RETURN(ENV_VALS(env));
  
end:
  
  CORE_RETURN("vals");
}

