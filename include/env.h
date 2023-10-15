// -*- c-backslash-column: 134; -*-

#pragma once

#include <stdbool.h>

#include "obj.h"
#include "list.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _env_set mode                                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
typedef enum {                                                                                                                       //
  GLOBAL,                                                                                                                            //
  LOCAL,                                                                                                                             //
  NEAREST,                                                                                                                           //
} ae_env_set_mode_t;                                                                                                                 //
                                                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros                                                                                                                //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_, symbols_, values_)                                                                                           \
  ({                                                                                                                                  \
    ae_obj_t * _obj = NEW(AE_ENV);                                                                                                    \
    _obj->parent    = (parent_);                                                                                                      \
    _obj->symbols   = (symbols_);                                                                                                     \
    _obj->values    = (values_);                                                                                                      \
    _obj;                                                                                                                             \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_PARENT(env)          ((env)->parent)                                                                                  //
#define    ENV_SYMS(env)            ((env)->symbols)                                                                                 //
#define    ENV_VALS(env)            ((env)->values)                                                                                  //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_ADD(env, sym, val)   (ae_env_add(            (env), (sym), (val)))                                                    //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_SET(env, sym, val)   (ae_env_set(   NEAREST, (env), (sym), (val)))                                                    //
#define    ENV_SET_G(env, sym, val) (ae_env_set(    GLOBAL, (env), (sym), (val)))                                                    //
#define    ENV_SET_L(env, sym, val) (ae_env_set(     LOCAL, (env), (sym), (val)))                                                    //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_FIND(env, sym)       (ae_env_lookup(NEAREST, (env), (sym), NULL))                                                     //
#define    ENV_FIND_G(env, sym)     (ae_env_lookup( GLOBAL, (env), (sym), NULL))                                                     //
#define    ENV_FIND_L(env, sym)     (ae_env_lookup(  LOCAL, (env), (sym), NULL))                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_BOUND_IN(mode, env, sym)                                                                                               \
  ({                                                                                                                                  \
    bool found = false;                                                                                                               \
    ((void)ae_env_lookup(mode, (env), (sym), &found));                                                                                \
    found;                                                                                                                            \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_BOUNDP(env, sym)     (ENV_BOUND_IN(NEAREST, env, sym))                                                                //
#define    ENV_BOUNDP_G(env, sym)   (ENV_BOUND_IN( GLOBAL, env, sym))                                                                //
#define    ENV_BOUNDP_L(env, sym)   (ENV_BOUND_IN(  LOCAL, env, sym))                                                                //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ENV_NEW_ROOT()           (ae_env_new_root())                                                                              //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define    ROOTP(env)               (ae_env_rootp((env)))                                                                            //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool       ae_env_rootp   (                        const ae_obj_t * const env                                                        );
void       ae_env_add     (                              ae_obj_t * const env,       ae_obj_t * const symbol, ae_obj_t * const value );
void       ae_env_set     (ae_env_set_mode_t mode,       ae_obj_t * const env,       ae_obj_t * const symbol, ae_obj_t * const value );
ae_obj_t * ae_env_lookup  (ae_env_set_mode_t mode,       ae_obj_t * const env, const ae_obj_t * const symbol, bool *     const found );
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_new_root(void                                                                                                      );
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
