#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_)                                                                           \
({                                                                                                 \
ae_obj_t * _obj = NEW(AE_ENV);                                                                     \
_obj->parent    = (parent_);                                                                       \
_obj->symbols   = NIL;                                                                             \
_obj->values    = NIL;                                                                             \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_PARENT(this)           ((this)->parent)
#define ENV_SYMS(this)             ((this)->symbols)
#define ENV_VALS(this)             ((this)->values)
#define ENV_FIND(this, sym)        (ae_env_find((this), (sym)))
#define ENV_ADD(this, sym, val)    (ae_env_add ((this), (sym), (val)))
#define ENV_SET(this, sym, val)    (ae_env_set ((this), (sym), (val)))
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_new_root(                                                                        );
ae_obj_t * ae_env_find    (ae_obj_t * const this, ae_obj_t * const symbol                          );
void       ae_env_add     (ae_obj_t * const this, ae_obj_t * const symbol, ae_obj_t * const value  );
void       ae_env_set     (ae_obj_t * const this, ae_obj_t * const symbol, ae_obj_t * const value  );
////////////////////////////////////////////////////////////////////////////////////////////////////
