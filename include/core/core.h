#pragma once

#include "obj.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN(DO)                                                                      \
  /*=============================================================================================*/\
  DO(exit)                                                                                         \
  DO(msleep)                                                                                       \
  DO(dobj)    /* debug data accessor   */                                                          \
  DO(errmsg)  /* error accessor        */                                                          \
  DO(errobj)  /* error accessor        */                                                          \
  DO(numer)   /* rational/int accessor */                                                          \
  DO(denom)   /* rational/int accessor */                                                          \
  DO(body)    /* fun accessor          */                                                          \
  DO(params)  /* fun accessor          */                                                          \
  DO(env)     /* env/fun accessor      */                                                          \
  DO(vals)    /* env accessor          */                                                          \
  DO(syms)    /* env accessor          */                                                          \
  DO(properp) /* reduceable            */                                                          \
  DO(tailp)   /* reduceable            */                                                          \
  DO(boundp)                                                                                       \
  DO(nl)                                                                                           \
  /*=============================================================================================*/\
  DO(put)     /* reducing this would require a core to get objs' addresses, might not bother. */   \
  DO(princ)   /* probably two out of these 3 could be reduced: */                                  \
  DO(print)                                                                                        \
  DO(write)                                                                                        \
  /*=============================================================================================*/\
  DO(type)                   /* GET_TYPE proxy */                                                  \
  DO(length)                 /* reduceable     */                                                  \
  DO(rplacd)                                                                                       \
  DO(rplaca)                                                                                       \
  DO(ahas)                                                                                         \
  DO(aset)                                                                                         \
  DO(aget)                                                                                         \
  DO(phas)                                                                                         \
  DO(pset)                                                                                         \
  DO(pget)                                                                                         \
  DO(not)                    /* reduceable */                                                      \
  DO(eql)                    /* reduceable */                                                      \
  DO(eq)                                                                                           \
  DO(cons)                                                                                         \
  DO(cdr)                                                                                          \
  DO(car)                                                                                          \
  DO(eval)                                                                                         \
  DO(list)                                                                                         \
  DO(set)                                                                                          \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_SPECIAL_CORE_FUN(DO)                                                              \
  DO(progn)                                                                                        \
  DO(setq)                                                                                         \
  DO(quote)                                                                                        \
  DO(macro)                                                                                        \
  DO(lambda)                                                                                       \
  DO(cond)                                                                                         \
  DO(let)                                                                                          \
  DO(let_star)                                                                                     \
  DO(if)                     /* reduceable */                                                      \
  DO(or)                     /* reduceable */                                                      \
  DO(and)                    /* reduceable */                                                      \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                  \
  DO(mod,   %, 1)                                                                                  \
  DO(div,   /, 1)            /* reducing these doesn't really seem like it would be worth the */   \
  DO(mul,   *, 1)            /* bother or the performance impact.                             */   \
  DO(sub,   -, 0)                                                                                  \
  DO(add,   +, 0)                                                                                  \
  DO(lsft, <<, 1)                                                                                  \
  DO(rsft, >>, 1)                                                                                  \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                   \
  DO(gt,     > , &=, true)                                                                         \
  DO(lt,     < , &=, true)  /* reducing these doesn't really seem like it would be worth the */    \
  DO(gte,    >=, &=, true)  /* bother or the performance impact.                             */    \
  DO(lte,    <=, &=, true)                                                                         \
  DO(nequal, !=, |=, false)                                                                        \
  DO(equal,  ==, &=, true)                                                                         \
 ///////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN(DECL_CORE);
FOR_EACH_SPECIAL_CORE_FUN(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////
