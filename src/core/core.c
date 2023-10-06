#include <stdbool.h>

#include "alist.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "obj.h"
#include "util.h"
#include "write.h"
#include "core_util_macros.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dobj(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("dobj");

  REQUIRE(env, args, (LENGTH(args) == 1));

#ifdef AE_DEBUG_OBJ
  CORE_RETURN("dobj", DOBJ(CAR(args)));
#else
  CORE_RETURN("dobj", NIL);
#endif
}

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
