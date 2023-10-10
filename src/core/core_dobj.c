#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dobj(
  __attribute__((unused)) ae_obj_t * const env,
  __attribute__((unused)) ae_obj_t * const args,
  __attribute__((unused)) int              args_length) {
  CORE_BEGIN("dobj");

#ifdef AE_DEBUG_OBJ
  CORE_RETURN("dobj", DOBJ(CAR(args)));
#else
  CORE_RETURN("dobj", NIL);
#endif
}
