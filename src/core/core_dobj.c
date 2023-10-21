#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_props(
  __attribute__((unused)) ae_obj_t * const env,
  __attribute__((unused)) ae_obj_t * const args,
  __attribute__((unused)) int              args_length) {
  CORE_BEGIN("props");

#ifdef AE_DEBUG_OBJ
  CORE_RETURN("props", DOBJ(CAR(args)));
#else
  CORE_RETURN("props", NIL);
#endif
}
