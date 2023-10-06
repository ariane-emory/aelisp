
#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// numeric comparison ops
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now.
#define DEF_CMP_OP(name, oper, assign, init)                                                       \
ae_obj_t * ae_core_ ## name(ae_obj_t * const env, ae_obj_t * const args) {                         \
  CORE_BEGIN(#name);                                                                               \
  assert(CONSP(args));                                                                             \
                                                                                                   \
  bool result = init;                                                                              \
                                                                                                   \
  FOR_EACH(elem, args) {                                                                           \
    if (NILP(CDR(position)))                                                                       \
        break;                                                                                     \
                                                                                                   \
    REQUIRE(env, args, INTEGERP(elem));                                                            \
    REQUIRE(env, args, INTEGERP(CADR(position)));                                                  \
                                                                                                   \
    result assign INT_VAL(elem) oper INT_VAL(CADR(position));                                      \
  }                                                                                                \
                                                                                                   \
  CORE_RETURN(#name, TRUTH(result));                                                               \
}

////////////////////////////////////////////////////////////////////////////////////////////////////

FOR_EACH_CORE_CMP_OP(DEF_CMP_OP);

