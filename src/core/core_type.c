#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _type
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_type(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("type");

  REQUIRE(env, args, (LENGTH(args) == 1));

  const char * type = GET_TYPE_STR(CAR(args));
  /* */ char * tmp  = free_list_malloc(strlen(type) + 2);

  sprintf(tmp, ":%s", type);

  ae_obj_t   * sym  = SYM(tmp);

  free_list_free(tmp);

  CORE_RETURN("type", sym);
}
