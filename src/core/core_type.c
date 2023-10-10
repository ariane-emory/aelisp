#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _type
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_type(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("type");

  const char * type = GET_TYPE_STR(CAR(args));
  /* */ char * tmp  = free_list_malloc(strlen(type) + 2);

  sprintf(tmp, ":%s", type);

  ae_obj_t   * sym  = SYM(tmp);

  free_list_free(tmp);

  CORE_RETURN("type", sym);
}
