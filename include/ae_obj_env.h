#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_EMV(parent)                                                                            \
({                                                                                                 \
assert(ENVP((parent)) || NILP((parent)));                                                          \
{ .metadata = AE_ENV, .parent = (parent), .symbols = NIL, .values = NIL };                         \
})
////////////////////////////////////////////////////////////////////////////////////////////////////


ae_obj_t *ae_env_find(ae_obj_t * const env, ae_obj_t * const var);
