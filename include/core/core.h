#pragma once

#include "obj.h"

#define FUNDEF_END NULL

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN(DO)                                                                                          \
  /*================================================================================================================*/ \
  DO(exit,     false,                                                         FUNDEF_END )                             \
  DO(msleep,   false,                                                         FUNDEF_END )                             \
  DO(dobj,     false, "debug-object",                                         FUNDEF_END ) /* debug data accessor   */ \
  DO(errmsg,   false, "error-message",                                        FUNDEF_END ) /* error accessor        */ \
  DO(errobj,   false, "error-object",                                         FUNDEF_END ) /* error accessor        */ \
  DO(numer,    false,                                                         FUNDEF_END ) /* rational/int accessor */ \
  DO(denom,    false,                                                         FUNDEF_END ) /* rational/int accessor */ \
  DO(body,     false,                                                         FUNDEF_END ) /* fun accessor          */ \
  DO(params,   false,                                                         FUNDEF_END ) /* fun accessor          */ \
  DO(env,      false,                                                         FUNDEF_END ) /* env/fun accessor      */ \
  DO(vals,     false,                                                         FUNDEF_END ) /* env accessor          */ \
  DO(syms,     false,                                                         FUNDEF_END ) /* env accessor          */ \
  DO(properp,  false, "proper?",                                              FUNDEF_END ) /* reduceable            */ \
  DO(tailp,    false, "tail?",                                                FUNDEF_END ) /* reduceable            */ \
  DO(boundp,   false, "bound?",                                               FUNDEF_END )                             \
  DO(nl,       false,                                                         FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(put,      false,                                                         FUNDEF_END )                             \
  DO(princ,    false,                                                         FUNDEF_END )                             \
  DO(print,    false,                                                         FUNDEF_END )                             \
  DO(write,    false,                                                         FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(type,     false,                                                         FUNDEF_END )        /* GET_TYPE proxy */ \
  DO(length,   false,                                                         FUNDEF_END )            /* reduceable */ \
  DO(rplacd,   false,                                                         FUNDEF_END )                             \
  DO(rplaca,   false,                                                         FUNDEF_END )                             \
  DO(ahas,     false,                                                         FUNDEF_END )                             \
  DO(aset,     false,                                                         FUNDEF_END )                             \
  DO(aget,     false,                                                         FUNDEF_END )                             \
  DO(phas,     false,                                                         FUNDEF_END )                             \
  DO(pset,     false,                                                         FUNDEF_END )                             \
  DO(pget,     false,                                                         FUNDEF_END )                             \
  DO(not,      false, "¬",                                                    FUNDEF_END )            /* reduceable */ \
  DO(eql,      false,                                                         FUNDEF_END )            /* reduceable */ \
  DO(eq,       false,                                                         FUNDEF_END )                             \
  DO(cons,     false,                                                         FUNDEF_END )                             \
  DO(cdr,      false,                                                         FUNDEF_END )                             \
  DO(car,      false,                                                         FUNDEF_END )                             \
  DO(eval,     false,                                                         FUNDEF_END )                             \
  DO(list,     false,                                                         FUNDEF_END )                             \
  DO(set,      false,                                                         FUNDEF_END )                             \
  DO(progn,    true,                                                          FUNDEF_END )                             \
  DO(setq,     true,  "≔",                                                        FUNDEF_END )                          \
  DO(quote,    true,                                                          FUNDEF_END )                             \
  DO(macro,    true,                                                          FUNDEF_END )                             \
  DO(lambda,   true,  "λ",                                                    FUNDEF_END )                             \
  DO(cond,     true,                                                          FUNDEF_END )                             \
  DO(let,      true,                                                          FUNDEF_END )                             \
  DO(let_str,  true,  "let*",                                                 FUNDEF_END )                             \
  DO(if,       true,                                                          FUNDEF_END )            /* reduceable */ \
  DO(or,       true,  "∨",                                                    FUNDEF_END )            /* reduceable */ \
  DO(and,      true,  "∧",                                                    FUNDEF_END )            /* reduceable */ \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                                      \
  DO(mod,   %, 1)                                                                                                      \
  DO(div,   /, 1)                            /* reducing these doesn't really seem like it would be worth the */       \
  DO(mul,   *, 1)                            /* bother or the performance impact.                             */       \
  DO(sub,   -, 0)                                                                                                      \
  DO(add,   +, 0)                                                                                                      \
  DO(lsft, <<, 1)                                                                                                      \
  DO(rsft, >>, 1)                                                                                                      \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                       \
  DO(gt,     > , &=, true)                                                                                             \
  DO(lt,     < , &=, true)                   /* reducing these doesn't really seem like it would be worth the */       \
  DO(gte,    >=, &=, true)                   /* bother or the performance impact.                             */       \
  DO(lte,    <=, &=, true)                                                                                             \
  DO(nequal, !=, |=, false)                                                                                            \
  DO(equal,  ==, &=, true)                                                                                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
