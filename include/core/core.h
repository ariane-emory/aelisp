#pragma once

#include "obj.h"

#define FUNDEF_END NULL
#define UNLIMITED  15

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_2(DO)                                                                                   \
  /*================================================================================================================*/  \
  DO(exit,         false,          0,          1,                              FUNDEF_END)                              \
  DO(allocated,    false,          0,          0,                              FUNDEF_END)                              \
  DO(load,         false,          1,          2,                              FUNDEF_END)                              \
  DO(read,         false,          1,          1,                              FUNDEF_END)                              \
  DO(requireb,     false,          1,          2, "require!",                  FUNDEF_END)                              \
  DO(require,      false,          1,          2,                              FUNDEF_END)                              \
  DO(sleep,        false,          1,          1,                              FUNDEF_END)                              \
  DO(sleep_us,     false,          1,          1, "sleep-us",                  FUNDEF_END)                              \
  DO(elapsed,      false,          1,          1,                              FUNDEF_END)                              \
  DO(elapsed_us,   false,          1,          1, "elapsed-us",                FUNDEF_END)                              \
  DO(now,          false,          0,          0,                              FUNDEF_END)                              \
  DO(now_us,       false,          0,          0, "now-us",                    FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(log_eval,     false,          0,          1, "log-eval",                  FUNDEF_END)                              \
  DO(log_core,     false,          0,          1, "log-core",                  FUNDEF_END)                              \
  DO(log_macro,    false,          0,          1, "log-macro",                 FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(error,        false,          1,          2,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(expand_path,  false,          1,          1, "expand-path",               FUNDEF_END)                              \
  DO(dirname,      false,          1,          1,                              FUNDEF_END)                              \
  DO(basename,     false,          1,          1,                              FUNDEF_END)                              \
  DO(pwd,          false,          0,          0,                              FUNDEF_END)                              \
  DO(cd,           false,          1,          1,                              FUNDEF_END)                              \
  DO(system,       false,          1,          1, "sys",                       FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(string,       false,          1,          1,                              FUNDEF_END) /* stringifier           */  \
  DO(intern,       false,          1,          1,                              FUNDEF_END) /* symbolizer            */  \
  DO(set_props,    false,          2,          2, "props!",                    FUNDEF_END) /* props accessor        */  \
  DO(props,        false,          1,          1,                              FUNDEF_END) /* props accessor        */  \
  DO(message,      false,          1,          1,                              FUNDEF_END) /* error accessor        */  \
  DO(name,         false,          1,          1, "symbol-name",               FUNDEF_END) /* symbol accessor       */  \
  DO(rational,     false,          2,          2,                              FUNDEF_END) /* rational constructor  */  \
  DO(denom,        false,          1,          1,                              FUNDEF_END) /* rational/int accessor */  \
  DO(numer,        false,          1,          1,                              FUNDEF_END) /* rational/int accessor */  \
  DO(body,         false,          1,          1,                              FUNDEF_END) /* fun accessor          */  \
  DO(params,       false,          1,          1,                              FUNDEF_END) /* fun accessor          */  \
  DO(env,          false,          0,          1,                              FUNDEF_END) /* env/fun accessor      */  \
  DO(vals,         false,          0,          1,                              FUNDEF_END) /* env accessor          */  \
  DO(syms,         false,          0,          1,                              FUNDEF_END) /* env accessor          */  \
  /*================================================================================================================*/  \
  DO(phas,         false,          2,          2, "plist-has?",                FUNDEF_END)                              \
  DO(psetb,        false,          3,          3, "plist-set!",                FUNDEF_END)                              \
  DO(pset,         false,          3,          3, "plist-set",                 FUNDEF_END)                              \
  DO(premoveb,     false,          2,          2, "plist-remove!",             FUNDEF_END)                              \
  DO(premove,      false,          2,          2, "plist-remove",              FUNDEF_END)                              \
  DO(pget,         false,          2,          2, "plist-get",                 FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(put,          false,          1,  UNLIMITED, "put-raw",                   FUNDEF_END)                              \
  DO(princ,        false,          1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(print,        false,          1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(write,        false,          1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(nl,           false,          0,          0,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(concat,       false,          0,  UNLIMITED,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(set,          false,          1,          2, "set!",                      FUNDEF_END)                              \
  DO(length,       false,          1,          1,                              FUNDEF_END)                              \
  DO(eval,         false,          1,          1,                              FUNDEF_END)                              \
  DO(apply,        true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(macro,        true,           2,  UNLIMITED,                              FUNDEF_END)                              \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_3(DO)                                                                                   \
  /*================================================================================================================*/  \
  DO(incrb,        true,           1,          1, "incr!",                     FUNDEF_END)                              \
  DO(decrb,        true,           1,          1, "decr!",                     FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(pushb,        true,           2,          2, "push!",                     FUNDEF_END)                              \
  DO(popb,         true,           1,          1, "pop!",                      FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(remove_prop,  false,          2,          2, "remove",                    FUNDEF_END)                              \
  DO(has_prop,     false,          2,          2, "has?",                      FUNDEF_END)                              \
  DO(put_prop,     false,          3,          3, "put",                       FUNDEF_END)                              \
  DO(get_prop,     false,          2,          2, "get",                       FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(properp,      false,          1,          1, "proper?",                   FUNDEF_END)                              \
  DO(tailp,        false,          1,          1, "tail?",                     FUNDEF_END)                              \
  DO(nilp,         false,          1,          1, "nil?",                      FUNDEF_END)                              \
  DO(boundp,       false,          1,          1, "bound?",                    FUNDEF_END)                              \
  DO(zerop,        false,          1,          1, "zero?",                     FUNDEF_END)                              \
  DO(onep,         false,          1,          1, "one?",                      FUNDEF_END)                              \
  DO(positivep,    false,          1,          1, "positive?",                 FUNDEF_END)                              \
  DO(negativep,    false,          1,          1, "negative?",                 FUNDEF_END)                              \
  DO(keywordp,     false,          1,          1, "keyword?",                  FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(bnot,         false,          1,          1, "~", "bit-not",              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(minus1,       false,          1,  UNLIMITED, "1-",                        FUNDEF_END)                              \
  DO(plus1,        false,          1,  UNLIMITED, "1+",                        FUNDEF_END)                              \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_MATH_OP(DO)                                                                                       \
  DO(bxor,  ^, 0, false, "bit-xor")                                                                                     \
  DO(band,  &, 0, false, "bit-and")                                                                                     \
  DO(bor,   |, 0, false, "bit-or" )                                                                                     \
  DO(rsft, >>, 1, false, "rsft"   )                                                                                     \
  DO(lsft, <<, 1, false, "lsft"   )                                                                                     \
  DO(mod,   %, 1, true,  "mod"    )                                                                                     \
  DO(div,   /, 1, true,  "div"    )                /* reducing these doesn't really seem like it would be worth the */  \
  DO(mul,   *, 1, false, "mul"    )                /* bother or the performance impact.                             */  \
  DO(sub,   -, 0, false, "sub"    )                                                                                     \
  DO(add,   +, 0, false, "add"    )                                                                                     \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_1(DO)                                                                                   \
  /*================================================================================================================*/  \
  DO(lambda,       true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(rplacd,       false,          2,          2, "rplacd!",                   FUNDEF_END)                              \
  DO(rplaca,       false,          2,          2, "rplaca!",                   FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(type,         false,          1,          1,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(setq,         true,           1,          2, "setq!",                     FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(id,           false,          1,          1,                              FUNDEF_END)                              \
  DO(not,          false,          1,          1,                              FUNDEF_END)                              \
  DO(or,           true,           1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(and,          true,           1,  UNLIMITED,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_CMP_OP(DO)                                                                                        \
  DO(lte,    <=, &=, true,  "lte")                                                                                      \
  DO(gte,    >=, &=, true,  "gte")                                                                                      \
  DO(lt,     < , &=, true,  "lt")                                                                                       \
  DO(gt,     > , &=, true,  "gt")                                                                                       \
  DO(nequal, !=, |=, false, "int-equal")                                                                                \
  DO(equal,  ==, &=, true,  "int-not-equal")                                                                            \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_CORE_FUN_GROUP_4(DO)                                                                                   \
  /*================================================================================================================*/  \
  DO(repeat,       true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(case,         true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(cond,         true,           1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(until,        true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(while,        true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(unless,       true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(when,         true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  DO(if,           true,           2,  UNLIMITED,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(list,         false,  UNLIMITED,  UNLIMITED,                              FUNDEF_END)                              \
  DO(quote,        true,           1,          1,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(letrec,       true,           1,  UNLIMITED,                              FUNDEF_END)                              \
  DO(let_star,     true,           1,  UNLIMITED, "let*",                      FUNDEF_END)                              \
  DO(let,          true,           1,  UNLIMITED,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(eql,          false,  UNLIMITED,  UNLIMITED, "eql?",                      FUNDEF_END)                              \
  DO(eq,           false,  UNLIMITED,  UNLIMITED, "eq?",                       FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(cons,         false,          2,          2,                              FUNDEF_END)                              \
  DO(cdr,          false,          1,          1,                              FUNDEF_END)                              \
  DO(car,          false,          1,          1,                              FUNDEF_END)                              \
  /*================================================================================================================*/  \
  DO(progn,        true,   UNLIMITED,  UNLIMITED,                              FUNDEF_END)                              \
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DECL_CORE(name, ...) ae_obj_t * ae_core_##name(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length);
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FOR_EACH_CORE_MATH_OP(DECL_CORE);
FOR_EACH_CORE_CMP_OP(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_1(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_2(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_3(DECL_CORE);
FOR_EACH_CORE_FUN_GROUP_4(DECL_CORE);
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Local Variables:
// c-backslash-column: 119
// c-syntactic-indentation-in-macros: nil
// End:
