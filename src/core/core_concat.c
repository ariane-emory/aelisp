#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");
  int total_length = 1;
  
  FOR_EACH(elem, args) {
    REQUIRE(env, args, STRINGP(elem));
    total_length += strlen(STR_VAL(elem));
  }

  SLOGF("Expect %d.\n", total_length);
  
  CORE_RETURN("concat", NIL);
}
