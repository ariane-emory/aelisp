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
#define ENV_PARENT(this)           ((this)->parent)
#define ENV_SYMS(this)             ((this)->symbols)
#define ENV_VALS(this)             ((this)->values)
#define ENV_FIND(this, member)     (ae_env_find((this), (member)))
#define ENV_ADD(this, sym, val)    (ae_env_add ((this), (sym), (val)))
#define ENV_SET(this, sym, val)    (ae_env_set ((this), (sym), (val)))
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_find(ae_obj_t * const this, ae_obj_t * const symbol                             );
ae_obj_t * ae_env_add (ae_obj_t *       this, ae_obj_t *       symbol, ae_obj_t * value           );
ae_obj_t * ae_env_set (ae_obj_t *       this, ae_obj_t *       symbol, ae_obj_t * value           );
////////////////////////////////////////////////////////////////////////////////////////////////////
