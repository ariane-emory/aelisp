
#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// numeric comparison ops
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now.
#define DEF_CMP_OP(name, lisp_name, oper, invert, fail_when, sym)                                                     \
  ae_obj_t * ae_core_ ## name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) { \
    CORE_BEGIN(#name);                                                                                                \
    assert(CONSP(args));                                                                                              \
                                                                                                                      \
    bool result = true;                                                                                               \
                                                                                                                      \
    FOR_EACH(elem, args) {                                                                                            \
      if (NILP(CDR(position)))                                                                                        \
        break;                                                                                                        \
                                                                                                                      \
      REQUIRE(INTEGERP(elem));                                                                                        \
      REQUIRE(INTEGERP(CADR(position)));                                                                              \
                                                                                                                      \
      bool tmp = INT_VAL(elem) oper INT_VAL(CADR(position));                                                          \
                                                                                                                      \
      printf("%u " #oper " %u = %u\n", INT_VAL(elem), INT_VAL(CADR(position)), tmp);                                  \
                                                                                                                      \
      result = tmp;                                                                                                   \
    }                                                                                                                 \
                                                                                                                      \
    if (invert) {                                                                                                     \
      printf("Invert!\n");                                                                                            \
                                                                                                                      \
      result = !result;                                                                                               \
    }                                                                                                                 \
                                                                                                                      \
    ret = TRUTH(result);                                                                                              \
                                                                                                                      \
    CORE_END(#name);                                                                                                  \
  }

////////////////////////////////////////////////////////////////////////////////////////////////////

FOR_EACH_CORE_CMP_OP(DEF_CMP_OP);

