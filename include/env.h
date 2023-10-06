#pragma once

#include <stdbool.h>

#include "obj.h"
#include "list.h"

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _env_set mode
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum {
  GLOBAL,
  LOCAL,
  NEAREST,
} ae_env_set_mode_t;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros                                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_, symbols_, values_)                                                                         \
({                                                                                                                  \
ae_obj_t * _obj = NEW(AE_ENV);                                                                                      \
_obj->parent    = (parent_);                                                                                        \
_obj->symbols   = (symbols_);                                                                                       \
_obj->values    = (values_);                                                                                        \
_obj;                                                                                                               \
})                                                                                                                  
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_PARENT(env)          ((env)->parent)                                                                   //
#define ENV_SYMS(env)            ((env)->symbols)                                                                  //
#define ENV_VALS(env)            ((env)->values)                                                                   //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_ADD(env, sym, val)   (ae_env_add(            (env), (sym), (val)))                                     //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_SET(env, sym, val)   (ae_env_set(   NEAREST, (env), (sym), (val)))                                     //
#define ENV_SET_G(env, sym, val) (ae_env_set(   NEAREST, (env), (sym), (val)))                                     //
#define ENV_SET_L(env, sym, val) (ae_env_set(   NEAREST, (env), (sym), (val)))                                     //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_FIND(env, sym)       (ae_env_lookup(NEAREST, (env), (sym), NULL))                                      //
#define ENV_FIND_G(env, sym)     (ae_env_lookup(NEAREST, (env), (sym), NULL))                                      //
#define ENV_FIND_L(env, sym)     (ae_env_lookup(NEAREST, (env), (sym), NULL))                                      //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_BOUNDP(env, sym)                                                                                        \
  ({                                                                                                                \
  bool found = false;                                                                                               \
  ((void)ae_env_lookup(NEAREST, (env), (sym), &found));                                                             \
  found;                                                                                                            \
  })
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENV_NEW_ROOT()         (ae_env_new_root())                                                                 //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ENVP(o)                ((! NULLP((o))) && (GET_TYPE((o)) == AE_ENV))                                       //
#define ROOTP(env)             (ENVP((env)) && (NILP(ENV_PARENT((env)))))                                          //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void       ae_env_add     (                        ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * value );
void       ae_env_set     (ae_env_set_mode_t mode, ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * value );
ae_obj_t * ae_env_lookup  (ae_env_set_mode_t mode, ae_obj_t * const env, ae_obj_t * const symbol, bool     * found );
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_new_root(void                                                                                    );
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
