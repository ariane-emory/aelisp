#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN(DO)                                                                      \
DO(ae_core_car)                                                                                    \
DO(ae_core_cdr)                                                                                    \
DO(ae_core_cons)                                                                                   \
DO(ae_core_eq)                                                                                     \
DO(ae_core_eql)                                                                                    \
DO(ae_core_atomp)                                                                                  \
DO(ae_core_not)                                                                                    \
DO(ae_core_print)                                                                                  \
DO(ae_core_princ)                                                                                  \
DO(ae_core_write) 

#define DECL_CORE_FUN(name) ae_obj_t * name  (ae_obj_t * const args);

FOR_EACH_CORE_FUN(DECL_CORE_FUN);
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
#define DECL_OP(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const args);
FOR_EACH_MATH_OP(DECL_OP);
FOR_EACH_CMP_OP(DECL_OP);
