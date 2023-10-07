#pragma once

#include "obj.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN(DO)                                                                                          \
  /*================================================================================================================*/ \
  DO(exit,     false)                                                                                                  \
  DO(msleep,   false)                                                                                                  \
  DO(dobj,     false)                        /* debug data accessor   */                                               \
  DO(errmsg,   false)                        /* error accessor        */                                               \
  DO(errobj,   false)                        /* error accessor        */                                               \
  DO(numer,    false)                        /* rational/int accessor */                                               \
  DO(denom,    false)                        /* rational/int accessor */                                               \
  DO(body,     false)                        /* fun accessor          */                                               \
  DO(params,   false)                        /* fun accessor          */                                               \
  DO(env,      false)                        /* env/fun accessor      */                                               \
  DO(vals,     false)                        /* env accessor          */                                               \
  DO(syms,     false)                        /* env accessor          */                                               \
  DO(properp,  false)                        /* reduceable            */                                               \
  DO(tailp,    false)                        /* reduceable            */                                               \
  DO(boundp,   false)                                                                                                  \
  DO(nl,       false)                                                                                                  \
  /*================================================================================================================*/ \
  DO(put,      false)                                                                                                  \
  DO(princ,    false)                                                                                                  \
  DO(print,    false)                                                                                                  \
  DO(write,    false)                                                                                                  \
  /*================================================================================================================*/ \
  DO(type,     false)                             /* GET_TYPE proxy */                                                 \
  DO(length,   false)                             /* reduceable     */                                                 \
  DO(rplacd,   false)                                                                                                  \
  DO(rplaca,   false)                                                                                                  \
  DO(ahas,     false)                                                                                                  \
  DO(aset,     false)                                                                                                  \
  DO(aget,     false)                                                                                                  \
  DO(phas,     false)                                                                                                  \
  DO(pset,     false)                                                                                                  \
  DO(pget,     false)                                                                                                  \
  DO(not,      false)                             /* reduceable     */                                                 \
  DO(eql,      false)                             /* reduceable     */                                                 \
  DO(eq,       false)                                                                                                  \
  DO(cons,     false)                                                                                                  \
  DO(cdr,      false)                                                                                                  \
  DO(car,      false)                                                                                                  \
  DO(eval,     false)                                                                                                  \
  DO(list,     false)                                                                                                  \
  DO(set,      false)                                                                                                  \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_SPECIAL(DO)                                                                                  \
  DO(progn)                                                                                                            \
  DO(setq)                                                                                                             \
  DO(quote)                                                                                                            \
  DO(macro)                                                                                                            \
  DO(lambda)                                                                                                           \
  DO(cond)                                                                                                             \
  DO(let)                                                                                                              \
  DO(let_star)                                                                                                         \
  DO(if)                                     /* reduceable */                                                          \
  DO(or)                                     /* reduceable */                                                          \
  DO(and)                                    /* reduceable */                                                          \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                                      \
  DO(mod,   %, 1)                                                                                                      \
  DO(div,   /, 1)                            /* reducing these doesn't really seem like it would be worth the */       \
  DO(mul,   *, 1)                            /* bother or the performance impact.                             */       \
  DO(sub,   -, 0)                                                                                                      \
  DO(add,   +, 0)                                                                                                      \
  DO(lsft, <<, 1)                                                                                                      \
  DO(rsft, >>, 1)                                                                                                      \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                       \
  DO(gt,     > , &=, true)                                                                                             \
  DO(lt,     < , &=, true)                   /* reducing these doesn't really seem like it would be worth the */       \
  DO(gte,    >=, &=, true)                   /* bother or the performance impact.                             */       \
  DO(lte,    <=, &=, true)                                                                                             \
  DO(nequal, !=, |=, false)                                                                                            \
  DO(equal,  ==, &=, true)                                                                                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN(DECL_CORE);
FOR_EACH_CORE_FUN_SPECIAL(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
