#pragma once

#include "ae_obj.h"
#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE(DO)                                                                          \
  DO(atomp)                                                                                        \
  DO(car)                                                                                          \
  DO(cdr)                                                                                          \
  DO(cons)                                                                                         \
  DO(eq)                                                                                           \
  DO(eql)                                                                                          \
  DO(not)                                                                                          \
  DO(sleep)                                                                                        \
  DO(write)                                                                                        \
  DO(print)                                                                                        \
  DO(princ) 
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_SPECIAL_FUN(DO)                                                              \
  DO(setq)                                                                                         \
  DO(progn)                                                                                        \
  DO(quote)                                                                                        \
  DO(lambda)                                                                                       \
  DO(macro)                                                                                        \
  DO(cond)                                                                                         \
  DO(if)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_MATH_OP(DO)                                                                       \
  DO(add, +, 0)                                                                                    \
  DO(sub, -, 0)                                                                                    \
  DO(mul, *, 1)                                                                                    \
  DO(div, /, 1)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CMP_OP(DO)                                                                        \
  DO(equal,  ==, &=, true)                                                                         \
  DO(nequal, !=, |=, false)                                                                        \
  DO(lt,     < , &=, true)                                                                         \
  DO(lte,    <=, &=, true)                                                                         \
  DO(gt,     > , &=, true)                                                                         \
  DO(gte,    >=, &=, true) 
////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name  (ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_MATH_OP(DECL_CORE);
FOR_EACH_CMP_OP(DECL_CORE);
FOR_EACH_CORE(DECL_CORE);
FOR_EACH_CORE_SPECIAL_FUN(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////
#define SETQ(env, sym, val) ae_core_setq(CONS(env, CONS(sym, LIST(val))))
// #define QUOTE(obj) ae_core_quote((obj))
