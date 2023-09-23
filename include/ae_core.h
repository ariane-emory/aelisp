#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN(DO)                                                                      \
  DO(car)                                                                                          \
  DO(cdr)                                                                                          \
  DO(cons)                                                                                         \
  DO(eq)                                                                                           \
  DO(eql)                                                                                          \
  DO(atomp)                                                                                        \
  DO(not)                                                                                          \
  DO(print)                                                                                        \
  DO(princ)                                                                                        \
  DO(write) 
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
#define DECL_CORE_FUN(name, ...) ae_obj_t * ae_core_##name  (ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_MATH_OP(DECL_CORE_FUN);
FOR_EACH_CMP_OP(DECL_CORE_FUN);
FOR_EACH_CORE_FUN(DECL_CORE_FUN);
////////////////////////////////////////////////////////////////////////////////////////////////////
