#pragma once

#include "ae_obj.h"
#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_, symbols_, values_)                                                        \
({                                                                                                 \
ae_obj_t * _obj = NEW(AE_ENV);                                                                     \
_obj->parent    = (parent_);                                                                       \
_obj->symbols   = (symbols_);                                                                      \
_obj->values    = (values_);                                                                       \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_PARENT(env)        ((env)->parent)
#define ENV_SYMS(env)          ((env)->symbols)
#define ENV_VALS(env)          ((env)->values)
#define ENV_FIND(env, sym)     (ae_env_find((env), (sym)))
#define ENV_ADD(env, sym, val) (ae_env_add ((env), (sym), (val)))
#define ENV_SET(env, sym, val) (ae_env_set ((env), (sym), (val)))
#define ENV_NEW_ROOT()         (ae_env_new_root())
/////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENVP(o)                 ((! NULLP((o))) && (GET_TYPE((o)) == AE_ENV))
#define ASSERT_ENVP(o)          (assert((ENVP(o))))
/////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_new_root(void                                                                    );
ae_obj_t * ae_env_find    (ae_obj_t * const env, ae_obj_t * const symbol                           );
void       ae_env_add     (ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value   );
void       ae_env_set     (ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value   );
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_define_list_fun(ae_obj_t * const env);
////////////////////////////////////////////////////////////////////////////////////////////////////
