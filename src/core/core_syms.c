#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _name
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_name(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("name");

  REQUIRE(env, args, SYMBOLP(CAR(args)));

  RETURN(NEW_STRING(SYM_VAL(CAR(args))));
  
  CORE_END("name");
}

