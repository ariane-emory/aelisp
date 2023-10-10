#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// math ops
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now. It mutates its first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_core_ ## name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {                         \
  CORE_BEGIN(#name);                                                                               \
  assert(CONSP(args));                                                                             \
                                                                                                   \
  long long int  accum  = 0;                                                                       \
  ae_obj_t * rest  = NIL;                                                                          \
                                                                                                   \
  if (NILP(CDR(args))) {                                                                           \
    accum = default;                                                                               \
    rest  = args;                                                                                  \
  }                                                                                                \
  else {                                                                                           \
    if (! INTEGERP(CAR(args))) {                                                                   \
      LOG(CAR(args), "NOT AN INTEGER");                                                            \
    }                                                                                              \
                                                                                                   \
    REQUIRE(env, args, INTEGERP(CAR(args)));                                                       \
                                                                                                   \
    accum = INT_VAL(CAR(args));                                                                    \
    rest  = CDR(args);                                                                             \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    if (! INTEGERP(CAR(args))) {                                                                   \
      LOG(CAR(args), "NOT AN INTEGER");                                                            \
    }                                                                                              \
                                                                                                   \
    REQUIRE(env, args, INTEGERP(elem));                                                            \
                                                                                                   \
    accum = accum oper INT_VAL(elem);                                                              \
  }                                                                                                \
                                                                                                   \
  CORE_RETURN(#name, NEW_INT(accum));                                                              \
}

////////////////////////////////////////////////////////////////////////////////////////////////////

FOR_EACH_CORE_MATH_OP(DEF_MATH_OP);

