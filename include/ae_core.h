#pragma once

#include "ae_obj.h"
#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE(DO)                                                                          \
  DO(car)                                                                                          \
  DO(cdr)                                                                                          \
  DO(rplaca)                                                                                       \
  DO(rplacd)                                                                                       \
  DO(cons)                                                                                         \
  DO(aget)                                                                                         \
  DO(aset)                                                                                         \
  DO(ahas)                                                                                         \
  DO(eq)                                                                                           \
  DO(exit)                                                                                         \
  DO(msleep)                                                                                       \
  /* DO(rplaca) */ /* Implement this one soon. */                                                  \
  /* DO(rplacd) */ /* Implement this one soon. */                                                  \
  /*=============================================================================================*/\
  DO(type)    /* GET_TYPE proxy */                                                                 \
  DO(body)    /* fun accessor */                                                                   \
  DO(params)  /* fun accessor */                                                                   \
  DO(syms)    /* env accessor */                                                                   \
  DO(vals)    /* env accessor */                                                                   \
  DO(env)     /* env/fun accessor */                                                               \
  DO(errmsg)  /* error accessor */                                                                 \
  DO(errobj)  /* error accessor */                                                                 \
  DO(numer)   /* rational/int accessor */                                                          \
  DO(denom)   /* rational/int accessor */                                                          \
  /*=============================================================================================*/\
  DO(atomp)   /* reduceable */                                                                     \
  DO(eql)     /* reduceable */                                                                     \
  DO(length)  /* reduceable */                                                                     \
  DO(not)     /* reduceable */                                                                     \
  DO(properp) /* reduceable */                                                                     \
  DO(tailp)   /* reduceable */                                                                     \
  DO(put)     /* reducing this would require a core to get objs' addresses, might not bother. */   \
  DO(princ)   /* probably two out of these final 3 could be reduced. */                            \
  DO(print)                                                                                        \
  DO(write)                                                                                        \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_SPECIAL_FUN(DO)                                                              \
  DO(cond)                                                                                         \
  DO(eval)                                                                                         \
  DO(lambda)                                                                                       \
  DO(macro)                                                                                        \
  DO(progn)                                                                                        \
  DO(quote)                                                                                        \
  DO(setq)                                                                                         \
  DO(if)      /* reduceable */                                                                     \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_MATH_OP(DO)                                                                       \
  DO(add, +, 0)             /* reducing these doesn't really seem like it would be worth the */    \
  DO(sub, -, 0)             /* bother or the performance impact.                             */    \
  DO(mul, *, 1)                                                                                    \
  DO(div, /, 1)                                                                                    \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CMP_OP(DO)                                                                        \
  DO(equal,  ==, &=, true)  /* reducing these doesn't really seem like it would be worth the */    \
  DO(nequal, !=, |=, false) /* bother or the performance impact.                             */    \
  DO(lt,     < , &=, true)                                                                         \
  DO(lte,    <=, &=, true)                                                                         \
  DO(gt,     > , &=, true)                                                                         \
  DO(gte,    >=, &=, true)                                                                         \
////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_MATH_OP(DECL_CORE);
FOR_EACH_CMP_OP(DECL_CORE);
FOR_EACH_CORE(DECL_CORE);
FOR_EACH_CORE_SPECIAL_FUN(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////

