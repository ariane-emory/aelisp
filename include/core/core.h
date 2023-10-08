#pragma once

#include "obj.h"

#define FUNDEF_END NULL

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_1(DO)                                                                                  \
  /*================================================================================================================*/ \
  DO(eql,      false, -1, -1,                                                 FUNDEF_END )            /* reduceable */ \
  DO(eq,       false, -1, -1,                                                 FUNDEF_END )                             \
  DO(cond,     true,  -1, -1,                                                 FUNDEF_END )                             \
  DO(progn,    true,  -1, -1,                                                 FUNDEF_END )                             \
  DO(cons,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(setq,     true,  -1, -1, "setq", "≔",                                    FUNDEF_END )                             \
  DO(aset,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(aget,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(list,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(quote,    true,  -1, -1,                                                 FUNDEF_END )                             \
  DO(not,      false, -1, -1, "not", "¬",                                     FUNDEF_END )            /* reduceable */ \
  DO(if,       true,  -1, -1,                                                 FUNDEF_END )            /* reduceable */ \
  DO(and,      true,  -1, -1, "and", "∧",                                     FUNDEF_END )            /* reduceable */ \
  DO(or,       true,  -1, -1, "or", "∨",                                      FUNDEF_END )            /* reduceable */ \
  DO(let_str,  true,  -1, -1, "let*",                                         FUNDEF_END )                             \
  DO(cdr,      false, -1, -1,                                                 FUNDEF_END )                             \
  DO(car,      false, -1, -1,                                                 FUNDEF_END )                             \
  DO(let,      true,  -1, -1,                                                 FUNDEF_END )                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                                      \
  DO(rsft, >>, 1)                                                                                                      \
  DO(lsft, <<, 1)                                                                                                      \
  DO(mod,   %, 1)                                                                                                      \
  DO(div,   /, 1)                            /* reducing these doesn't really seem like it would be worth the */       \
  DO(mul,   *, 1)                            /* bother or the performance impact.                             */       \
  DO(sub,   -, 0)                                                                                                      \
  DO(add,   +, 0)                                                                                                      \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_2(DO)                                                                                  \
  /*================================================================================================================*/ \
  DO(exit,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(msleep,   false, -1, -1,                                                 FUNDEF_END )                             \
  DO(dobj,     false, -1, -1, "debug-object",                                 FUNDEF_END ) /* debug data accessor   */ \
  DO(errmsg,   false, -1, -1, "error-message",                                FUNDEF_END ) /* error accessor        */ \
  DO(errobj,   false, -1, -1, "error-object",                                 FUNDEF_END ) /* error accessor        */ \
  DO(numer,    false, -1, -1,                                                 FUNDEF_END ) /* rational/int accessor */ \
  DO(denom,    false, -1, -1,                                                 FUNDEF_END ) /* rational/int accessor */ \
  DO(body,     false, -1, -1,                                                 FUNDEF_END ) /* fun accessor          */ \
  DO(params,   false, -1, -1,                                                 FUNDEF_END ) /* fun accessor          */ \
  DO(env,      false, -1, -1,                                                 FUNDEF_END ) /* env/fun accessor      */ \
  DO(vals,     false, -1, -1,                                                 FUNDEF_END ) /* env accessor          */ \
  DO(syms,     false, -1, -1,                                                 FUNDEF_END ) /* env accessor          */ \
  DO(properp,  false, -1, -1, "proper?",                                      FUNDEF_END ) /* reduceable            */ \
  DO(tailp,    false, -1, -1, "tail?",                                        FUNDEF_END ) /* reduceable            */ \
  DO(boundp,   false, -1, -1, "bound?",                                       FUNDEF_END )                             \
  DO(nl,       false, -1, -1,                                                 FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(put,      false, -1, -1,                                                 FUNDEF_END )                             \
  DO(princ,    false, -1, -1,                                                 FUNDEF_END )                             \
  DO(print,    false, -1, -1,                                                 FUNDEF_END )                             \
  DO(write,    false, -1, -1,                                                 FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(eval,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(macro,    true,  -1, -1,                                                 FUNDEF_END )                             \
  DO(set,      false, -1, -1,                                                 FUNDEF_END )                             \
  DO(type,     false, -1, -1,                                                 FUNDEF_END ) /* GET_TYPE proxy        */ \
  DO(length,   false, -1, -1,                                                 FUNDEF_END ) /* reduceable            */ \
  DO(rplacd,   false, -1, -1,                                                 FUNDEF_END )                             \
  DO(rplaca,   false, -1, -1,                                                 FUNDEF_END )                             \
  DO(phas,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(pset,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(pget,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(ahas,     false, -1, -1,                                                 FUNDEF_END )                             \
  DO(lambda,   true,  -1, -1, "lambda", "λ",                                  FUNDEF_END )                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                       \
  DO(gt,     > , &=, true)                                                                                             \
  DO(lt,     < , &=, true)                   /* reducing these doesn't really seem like it would be worth the */       \
  DO(gte,    >=, &=, true)                   /* bother or the performance impact.                             */       \
  DO(lte,    <=, &=, true)                                                                                             \
  DO(nequal, !=, |=, false)                                                                                            \
  DO(equal,  ==, &=, true)                                                                                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_1(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_2(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
