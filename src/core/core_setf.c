#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setf
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setf(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
#ifdef AE_LOG_CORE
  // No CORE_BEGIN!
  LOG(args, "[core_setf]");
  INDENT;
#endif

  ae_obj_t * result = ae_core_setq(env, args, args_length);
  
  CORE_RETURN("setf", result);
}
