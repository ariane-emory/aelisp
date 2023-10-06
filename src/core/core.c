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
