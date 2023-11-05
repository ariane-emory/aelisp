#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _incrb
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_incrb(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("pop!");

  ae_obj_t * const sym  = CAR(args);

  REQUIRE(env, args, SETTABLEP(sym) && ENV_BOUNDP(env, sym),
          "incrb! only works on bound and settable symbols");

  ae_obj_t * const lst  = RETURN_IF_ERRORP(EVAL(env, sym));

  REQUIRE(env, args, CONSP(lst));

  ae_obj_t * const tail = CDR(lst);
  
  ret = CAR(lst);

  ENV_SET(env, sym, tail);

end:
  
  CORE_RETURN("pop!", ret);
}

