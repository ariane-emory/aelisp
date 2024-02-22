
#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// numeric comparison ops
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now.
#define DEF_CMP_OP(name, oper, assign, init, sym)                                                                     \
  ae_obj_t * ae_core_ ## name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) { \
    CORE_BEGIN(#name);                                                                                                \
    assert(CONSP(args));                                                                                              \
                                                                                                                      \
    bool result = init;                                                                                               \
                                                                                                                      \
    FOR_EACH(elem, args) {                                                                                            \
      if (NILP(CDR(position)))                                                                                        \
        break;                                                                                                        \
                                                                                                                      \
      REQUIRE(INTEGERP(elem));                                                                                        \
      REQUIRE(INTEGERP(CADR(position)));                                                                              \
                                                                                                                      \
      printf("\nResult = %d\n", result);                                                                              \
      printf("Compare %u and %u with " #oper "\n", INT_VAL(elem), INT_VAL(CADR(position)));                           \
                                                                                                                      \
      bool tmp = INT_VAL(elem) oper INT_VAL(CADR(position));                                                          \
                                                                                                                      \
      result assign tmp;                                                                                              \
                                                                                                                      \
      if (result != init)                                                                                             \
        break;                                                                                                        \
    }                                                                                                                 \
                                                                                                                      \
    ret = TRUTH(result);                                                                                              \
                                                                                                                      \
    CORE_END(#name);                                                                                                  \
  }

////////////////////////////////////////////////////////////////////////////////////////////////////

FOR_EACH_CORE_CMP_OP(DEF_CMP_OP);

