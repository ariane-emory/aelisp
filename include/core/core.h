#pragma once

#include "obj.h"

#define FUNDEF_END NULL
#define UNLIMITED  15

extern bool log_core;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_1(DO)                                                                                     \
  /*================================================================================================================*/    \
  DO(lambda,       true,           2,  UNLIMITED, "λ", "lambda",               FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(rplacd,       false,          2,          2, "setcdr!", "rplacd!",        FUNDEF_END)                                \
  DO(rplaca,       false,          2,          2, "setcar!", "rplaca!",        FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(type,         false,          1,          1,                              FUNDEF_END) /* GET_TYPE proxy        */    \
  /*================================================================================================================*/    \
  DO(setq,         true,           1,          2, "setq!",                     FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(not,          false,          1,  UNLIMITED, "¬", "not",                  FUNDEF_END)                                \
  DO(or,           true,           2,  UNLIMITED, "∨", "or",                   FUNDEF_END)                                \
  DO(and,          true,           2,  UNLIMITED, "∧", "and",                  FUNDEF_END)                                \
  /*================================================================================================================*/    \

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_4(DO)                                                                                     \
  DO(until,        true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(while,        true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(unless,       true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(when,         true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(if,           true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(cond,         true,           1,  UNLIMITED,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(list,         false,  UNLIMITED,  UNLIMITED,                              FUNDEF_END)                                \
  DO(quote,        true,           1,          1,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(letrec,       true,           1,  UNLIMITED,                              FUNDEF_END)                                \
  DO(let_star,     true,           1,  UNLIMITED, "let*",                      FUNDEF_END)                                \
  DO(let,          true,           1,  UNLIMITED,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(eql,          false,  UNLIMITED,  UNLIMITED, "eql?",                      FUNDEF_END)                                \
  DO(eq,           false,  UNLIMITED,  UNLIMITED, "eq?",                       FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(cons,         false,          2,          2,                              FUNDEF_END)                                \
  DO(cdr,          false,          1,          1,                              FUNDEF_END)                                \
  DO(car,          false,          1,          1,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(progn,        true,   UNLIMITED,  UNLIMITED,                              FUNDEF_END)                                \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_3(DO)                                                                                     \
  /*================================================================================================================*/    \
  DO(has_prop,     true,           2,          2, "has?",                      FUNDEF_END)                                \
  DO(set_prop,     true,           3,          3, "put!",                      FUNDEF_END)                                \
  DO(get_prop,     true,           2,          2, "get",                       FUNDEF_END)                                \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_2(DO)                                                                                     \
  /*================================================================================================================*/    \
  DO(exit,         false,          0,          1,                              FUNDEF_END)                                \
  DO(load,         false,          1,          1,                              FUNDEF_END)                                \
  DO(sleep,        false,          1,          1,                              FUNDEF_END)                                \
  DO(elapsed,      false,          1,          1,                              FUNDEF_END)                                \
  DO(elapsed_us,   false,          1,          1, "elapsed-us",                FUNDEF_END)                                \
  DO(now,          false,          0,          0,                              FUNDEF_END)                                \
  DO(now_us,       false,          0,          0, "now_us",                    FUNDEF_END)                                \
  DO(program,      false,          0,          0,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(log_eval,     false,          0,          1, "log-eval",                  FUNDEF_END)                                \
  DO(log_core,     false,          0,          1, "log-core",                  FUNDEF_END)                                \
  DO(log_macro,    false,          0,          1, "log-macro",                 FUNDEF_END)                                \
  DO(log_all,      false,          0,          1, "log-all",                   FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(concat,       false,          0,  UNLIMITED,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(error,        false,          1,          2,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(props,        true,           1,          1,                              FUNDEF_END) /* props accessor        */    \
  DO(errmsg,       false,          1,          1, "error-message",             FUNDEF_END) /* error accessor        */    \
  DO(name,         false,          1,          1, "symbol-name",               FUNDEF_END) /* symbol accessor       */    \
  DO(errobj,       false,          1,          1, "error-object",              FUNDEF_END) /* error accessor        */    \
  DO(denom,        false,          1,          1,                              FUNDEF_END) /* rational/int accessor */    \
  DO(numer,        false,          1,          1,                              FUNDEF_END) /* rational/int accessor */    \
  DO(body,         false,          1,          1,                              FUNDEF_END) /* fun accessor          */    \
  DO(params,       false,          1,          1,                              FUNDEF_END) /* fun accessor          */    \
  DO(env,          false,          0,          1,                              FUNDEF_END) /* env/fun accessor      */    \
  DO(vals,         false,          0,          1,                              FUNDEF_END) /* env accessor          */    \
  DO(syms,         false,          0,          1,                              FUNDEF_END) /* env accessor          */    \
  /*================================================================================================================*/    \
  DO(repeat,       true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(properp,      false,          1,          1, "proper?",                   FUNDEF_END)                                \
  DO(tailp,        false,          1,          1, "tail?",                     FUNDEF_END)                                \
  DO(nilp,         false,          1,          1, "nil?",                      FUNDEF_END)                                \
  DO(boundp,       false,          1,          1, "bound?",                    FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(phas,         false,          2,          2, "phas?",                     FUNDEF_END)                                \
  DO(pset,         false,          3,          3,                              FUNDEF_END)                                \
  DO(pget,         false,          2,          2,                              FUNDEF_END)                                \
  DO(ahas,         false,          2,          2, "ahas?",                     FUNDEF_END)                                \
  DO(aset,         false,          3,          3,                              FUNDEF_END)                                \
  DO(aget,         false,          2,          2,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(put,          false,          1,  UNLIMITED, "put-raw",                   FUNDEF_END)                                \
  DO(princ,        false,          1,  UNLIMITED,                              FUNDEF_END)                                \
  DO(print,        false,          1,  UNLIMITED,                              FUNDEF_END)                                \
  DO(write,        false,          1,  UNLIMITED,                              FUNDEF_END)                                \
  DO(nl,           false,          0,          0,                              FUNDEF_END)                                \
  /*================================================================================================================*/    \
  DO(set,          false,          1,          2, "set!",                      FUNDEF_END)                                \
  DO(length,       false,          1,          1,                              FUNDEF_END) /* reduceable            */    \
  DO(eval,         false,          1,          1,                              FUNDEF_END)                                \
  DO(apply,        true,           2,  UNLIMITED,                              FUNDEF_END)                                \
  DO(macro,        true,           2,  UNLIMITED,                              FUNDEF_END)                                \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                                         \
  DO(rsft, >>, 1)                                                                                                         \
  DO(lsft, <<, 1)                                                                                                         \
  DO(mod,   %, 1)                                                                                                         \
  DO(div,   /, 1)                            /* reducing these doesn't really seem like it would be worth the */          \
  DO(mul,   *, 1)                            /* bother or the performance impact.                             */          \
  DO(sub,   -, 0)                                                                                                         \
  DO(add,   +, 0)                                                                                                         \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                          \
  DO(lte,    <=, &=, true)                                                                                                \
  DO(gte,    >=, &=, true)                         /* bother or the performance impact.                             */    \
  DO(lt,     < , &=, true)                         /* reducing these doesn't really seem like it would be worth the */    \
  DO(gt,     > , &=, true)                                                                                                \
  DO(nequal, !=, |=, false)                                                                                               \
  DO(equal,  ==, &=, true)                                                                                                \
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_1(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_2(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_3(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_4(DECL_CORE);
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Local Variables:
// c-backslash-column: 119
// c-syntactic-indentation-in-macros: nil
// End:
