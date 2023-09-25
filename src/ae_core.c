#include <stdbool.h>

#include "ae_core.h"
#include "ae_list.h"
#include "ae_eval.h"
#include "ae_env.h"
#include "ae_write.h"
#include "ae_util.h"

#define SPECIAL_FUN_ARGS(env, args, bundle)                                                        \
  ASSERT_CONSP(env_and_args);                                                                      \
  ASSERT_ENVP(CAR(env_and_args));                                                                  \
  ASSERT_TAILP(CDR(env_and_args));                                                                 \
                                                                                                   \
  ae_obj_t * env  = CAR(env_and_args);                                                             \
  ae_obj_t * args = CDR(bundle)

#ifdef AE_LOG_CORE
#  define LOG_CREATE_LAMBDA_OR_MACRO(name)                                                         \
  PR("\n[CREATE " name "]");                                                                       \
  LOG(args, "create args");                                                                        \
  LOG(CAR(args), "fun args");                                                                      \
  LOG(CDR(args), "fun body");                                                                      \
  NL
#else
#  define LOG_CREATE_LAMBDA_OR_MACRO(name) ((void)0)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// math
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now. It mutates its first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_core_##name(ae_obj_t * const args) {                                                 \
  ASSERT_CONSP(args);                                                                              \
                                                                                                   \
  ae_obj_t * accum = NIL;                                                                          \
  ae_obj_t * rest  = NIL;                                                                          \
                                                                                                   \
  if (NILP(CDR(args))) {                                                                           \
    accum = NEW_INT(default);                                                                      \
    rest = args;                                                                                   \
  }                                                                                                \
  else {                                                                                           \
    ASSERT_INTEGERP(CAR(args));                                                                    \
                                                                                                   \
    accum = CAR(args);                                                                             \
    rest = CDR(args);                                                                              \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    ASSERT_INTEGERP(elem);                                                                         \
                                                                                                   \
    INT_VAL(accum) = INT_VAL(accum) oper INT_VAL(elem);                                            \
  }                                                                                                \
                                                                                                   \
  return accum;                                                                                    \
}

FOR_EACH_MATH_OP(DEF_MATH_OP);

////////////////////////////////////////////////////////////////////////////////////////////////////
// numeric comparison
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now.
#define DEF_CMP_OP(name, oper, assign, init)                                                       \
ae_obj_t * ae_core_##name(ae_obj_t * const args) {                                                 \
  ASSERT_CONSP(args);                                                                              \
                                                                                                   \
  bool result = init;                                                                              \
                                                                                                   \
  FOR_EACH(elem, args) {                                                                           \
    if (NILP(CDR(position)))                                                                       \
        break;                                                                                     \
                                                                                                   \
    ASSERT_INTEGERP(elem);                                                                         \
    ASSERT_INTEGERP(CADR(position));                                                               \
                                                                                                   \
    result assign INT_VAL(elem) oper INT_VAL(CADR(position));                                      \
  }                                                                                                \
                                                                                                   \
  return ae_obj_truth(result);                                                                     \
}

FOR_EACH_CMP_OP(DEF_CMP_OP);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

#ifdef AE_LOG_CORE
  PR("\n[core setq]");
  OLOG(env_and_args);
  OLOG(env);
  OLOG(args);
#endif
 
  ASSERT_SYMBOLP(CAR(args));
  ASSERT_CONSP(CDR(args));

  ae_obj_t * sym  = CAR(args);
  ae_obj_t * val  = EVAL(env, CADR(args)); // allowed to be NIL.

  ENV_SET(env, sym, val);

  return val;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

#ifdef AE_LOG_CORE
  PR("\n[core progn]");
  LOG(env, "progn env");
  LOG(args, "progn args");
  NL;
#endif

  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args)
    ret = EVAL(env, elem);

  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _quote
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_quote(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);
  ASSERT_NILP(CDR(args)); // for now, this supports 1 argument.

  return CAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  LOG_CREATE_LAMBDA_OR_MACRO("LAMBDA");

  assert(TAILP(CAR(args)) || SYMBOLP(CAR(args)));
  ASSERT_TAILP(CDR(args));

  return NEW_LAMBDA(CAR(args), CDR(args), env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  LOG_CREATE_LAMBDA_OR_MACRO("macro");
  
  ASSERT_TAILP(CAR(args));
  ASSERT_TAILP(CDR(args));

  return NEW_MACRO(CAR(args), CDR(args), env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cond
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cond(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

#ifdef AE_LOG_CORE
  NL;
  PR("cond? ");
  WRITE(args);
#endif

  if (NILP(args))
    return NIL;

  ASSERT_CONSP(CAR(args));

  ae_obj_t * caar = CAAR(args);
  ae_obj_t * cdar = CDAR(args);

#ifdef AE_LOG_CORE
  NL;
  PR("caar ");
  WRITE(caar);
  NL;
  PR("cdar  ");
  WRITE(cdar);
#endif

  if (NOT_NILP(EVAL(env, caar)))
    return EVAL(env, ae_core_progn(CONS(env, cdar)));
  return ae_core_cond(CONS(env, CDR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_if(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

#ifdef AE_LOG_CORE
  NL;
  PR("if:          ");
  PRINC(CAR(args));
  NL;
  PR("args:        ");
  PRINC(args);
  NL;
  PR("then:        ");
  PRINC(CADR(args));
  NL;
  PR("else:        ");
  PRINC(CONS(INTERN("progn"), CDDR(args)));
  NL;
#endif

  // ASSERT_NOT_NILP(CAR(args));
  // ASSERT_NOT_NILP(CADR(args));

  bool cond_result = NOT_NILP(EVAL(env, CAR(args)));

#ifdef AE_LOG_CORE
  PR("cond_result: ");
  PRINC(cond_result ? TRUE : NIL);
  NL;
#endif

  if (cond_result) {
#ifdef AE_LOG_CORE
    PR("Choose then.\n");
#endif
    return EVAL(env, CADR(args));
  }
  else {
#ifdef AE_LOG_CORE
    PR("Choose else.\n");
#endif
    return EVAL(env, CONS(INTERN("progn"), CDDR(args)));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NILP(CDR(args));  // takes only one arg.
  ASSERT_TAILP(CAR(args)); // which must be a tail.

  if (NILP(CAR(args)))     // car of nil is nil.
    return NIL;

  return CAAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NILP(CDR(args));  // takes only one arg.
  ASSERT_TAILP(CAR(args)); // which must be a tail.

  if (NILP(CAR(args)))     // cdr of nil is nil.
    return NIL;

  return CDAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_TAILP(CDR(args)); // 2nd arg must be a tail.
  ASSERT_NILP(CDDR(args)); // only 2 args.

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);

  return CONS(head, tail);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NOT_NILP(CDR(args));

  FOR_EACH(tailarg, CDR(args))
    if (NEQ(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NOT_NILP(CDR(args));

  FOR_EACH(tailarg, CDR(args))
    if (NEQL(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _atomp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_atomp(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  FOR_EACH(elem, args)
    if (! ATOMP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  FOR_EACH(elem, args)
    if (NOT_NILP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  NL;

  int written = 1;

  FOR_EACH(elem, args) {
    written += PRINC(elem);

    if (NOT_NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  int written = 0;

  FOR_EACH(elem, args) {
    written += PRINC(elem);
  }

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

// _write is temporarily identical to _princ.

ae_obj_t * ae_core_write(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  int written = 0;

  FOR_EACH(elem, args) {
    written += WRITE(elem);
    if (NOT_NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  return NEW_INT(written);
}
