#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_lisp_car  (ae_obj_t * const args);
ae_obj_t * ae_lisp_cdr  (ae_obj_t * const args);
ae_obj_t * ae_lisp_cons (ae_obj_t * const args);
ae_obj_t * ae_lisp_eq   (ae_obj_t * const args);
ae_obj_t * ae_lisp_eql  (ae_obj_t * const args);
ae_obj_t * ae_lisp_atomp(ae_obj_t * const args);
ae_obj_t * ae_lisp_not  (ae_obj_t * const args);
ae_obj_t * ae_lisp_print(ae_obj_t * const args);
ae_obj_t * ae_lisp_princ(ae_obj_t * const args);
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
#define DECL_OP(name, ...) ae_obj_t * ae_lisp_##name(ae_obj_t * const args);
FOR_EACH_MATH_OP(DECL_OP);
FOR_EACH_CMP_OP(DECL_OP);
