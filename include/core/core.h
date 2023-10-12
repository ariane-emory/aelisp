#pragma once

#include "obj.h"

#define FUNDEF_END NULL
#define UNLIMITED  15

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_1(DO)                                                                                  \
  /*================================================================================================================*/ \
  DO(phas,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(pset,     false,          3,          3,                                 FUNDEF_END )                             \
  DO(pget,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(aset,     false,          3,          3,                                 FUNDEF_END )                             \
  DO(aget,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(ahas,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(eql,      false,  UNLIMITED,  UNLIMITED, "eql", "eql?",                  FUNDEF_END ) /* reduceable            */ \
  DO(eq,       false,  UNLIMITED,  UNLIMITED, "eq",  "eq?",                   FUNDEF_END )                             \
  DO(cond,     true,           1,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(progn,    true,   UNLIMITED,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(setq,     true,           2,          2, "setq", "≔",                    FUNDEF_END )                             \
  DO(list,     false,  UNLIMITED,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(quote,    true,           1,          1,                                 FUNDEF_END )                             \
  DO(cons,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(not,      false,          1,  UNLIMITED, "not", "¬", "nil?",             FUNDEF_END ) /* reduceable            */ \
  DO(if,       true,           2,  UNLIMITED,                                 FUNDEF_END ) /* reduceable            */ \
  DO(when,     true,           2,  UNLIMITED,                                 FUNDEF_END ) /* reduceable            */ \
  DO(and,      true,           2,          2, "and", "∧",                     FUNDEF_END ) /* reduceable            */ \
  DO(or,       true,           2,          2, "or", "∨",                      FUNDEF_END ) /* reduceable            */ \
  DO(let_str,  true,           2,  UNLIMITED, "let*",                         FUNDEF_END )                             \
  DO(let,      true,           2,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(cdr,      false,          1,          1,                                 FUNDEF_END )                             \
  DO(car,      false,          1,          1,                                 FUNDEF_END )                             \
  DO(elapsed,  false,          1,          1,                                 FUNDEF_END )                             \
  DO(time,     false,          0,          0,                                 FUNDEF_END )                             \
  DO(repeat,   true,           2,  UNLIMITED,                                 FUNDEF_END )                             \
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
  DO(concat,   false,          0,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(exit,     false,          0,          1,                                 FUNDEF_END )                             \
  DO(sleep,    false,          1,          1,                                 FUNDEF_END )                             \
  DO(dobj,     false,          1,          1, "dobj", "debug-object",         FUNDEF_END ) /* debug data accessor   */ \
  DO(error,    false,          1,          2,                                 FUNDEF_END )                             \
  DO(errmsg,   false,          1,          1, "errmsg", "error-message",      FUNDEF_END ) /* error accessor        */ \
  DO(name,     false,          1,          1, "sym-name",                     FUNDEF_END ) /* symbol accessor       */ \
  DO(errobj,   false,          1,          1, "errobj", "error-object",       FUNDEF_END ) /* error accessor        */ \
  DO(type,     false,          1,          1,                                 FUNDEF_END ) /* GET_TYPE proxy        */ \
  DO(body,     false,          1,          1,                                 FUNDEF_END ) /* fun accessor          */ \
  DO(params,   false,          1,          1,                                 FUNDEF_END ) /* fun accessor          */ \
  DO(env,      false,          0,          1,                                 FUNDEF_END ) /* env/fun accessor      */ \
  DO(vals,     false,          1,          1,                                 FUNDEF_END ) /* env accessor          */ \
  DO(syms,     false,          1,          1,                                 FUNDEF_END ) /* env accessor          */ \
  DO(denom,    false,          1,          1,                                 FUNDEF_END ) /* rational/int accessor */ \
  DO(numer,    false,          1,          1,                                 FUNDEF_END ) /* rational/int accessor */ \
  DO(properp,  false,          1,          1, "proper?",                      FUNDEF_END ) /* reduceable            */ \
  DO(tailp,    false,          1,          1, "tail?",                        FUNDEF_END ) /* reduceable            */ \
  DO(boundp,   false,          1,          1, "bound?",                       FUNDEF_END )                             \
  DO(nl,       false,          0,          0,                                 FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(put,      false,          1,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(princ,    false,          1,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(print,    false,          1,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(write,    false,          1,  UNLIMITED,                                 FUNDEF_END )                             \
  /*================================================================================================================*/ \
  DO(eval,     false,          1,          1,                                 FUNDEF_END )                             \
  DO(macro,    true,           2,  UNLIMITED,                                 FUNDEF_END )                             \
  DO(setf,     false,          2,          2,                                 FUNDEF_END )                             \
  DO(length,   false,          1,          1,                                 FUNDEF_END ) /* reduceable            */ \
  DO(rplacd,   false,          2,          2,                                 FUNDEF_END )                             \
  DO(rplaca,   false,          2,          2,                                 FUNDEF_END )                             \
  DO(lambda,   true,           2,  UNLIMITED, "lambda", "λ",                  FUNDEF_END )                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                       \
  DO(gt,     > , &=, true)                                                                                             \
  DO(lt,     < , &=, true)                         /* reducing these doesn't really seem like it would be worth the */ \
  DO(gte,    >=, &=, true)                         /* bother or the performance impact.                             */ \
  DO(lte,    <=, &=, true)                                                                                             \
  DO(nequal, !=, |=, false)                                                                                            \
  DO(equal,  ==, &=, true)                                                                                             \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_1(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_2(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
