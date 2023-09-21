#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_)                                                                           \
({                                                                                                 \
ae_obj_t * env = NEW(AE_ENV);                                                                      \
env->parent    = (parent_);                                                                        \
env->symbols   = NIL;                                                                              \
env->values    = NIL;                                                                              \
env;                                                                                               \
})

////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t *ae_env_find(ae_obj_t * const env, ae_obj_t * const var);
