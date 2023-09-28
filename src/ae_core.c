#include <stdbool.h>
#include <unistd.h>

#include "ae_core.h"
#include "ae_list.h"
#include "ae_eval.h"
#include "ae_env.h"
#include "ae_write.h"
#include "ae_util.h"
#include "ae_free_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define REQUIRE(obj, cond)                                                                         \
  if (! (cond)) {                                                                                  \
    char * cond_str = #cond;                                                                       \
    char * msg = free_list_malloc(256);                                                            \
    sprintf(msg, "%s:%d: \"Error in %s: require %s!\"", __FILE__, __LINE__, __func__, cond_str);   \
    return NEW_ERROR(obj, msg);                                                                    \
  }

#define SPECIAL_FUN_ARGS(env, args, bundle)                                                        \
  assert(CONSP(env_and_args));                                                                     \
  assert(ENVP(CAR(env_and_args)));                                                                 \
  assert(TAILP(CDR(env_and_args)));                                                                \
  ae_obj_t * env  = CAR(env_and_args);                                                             \
  ae_obj_t * args = CDR(bundle)                                                                    \

#ifdef AE_LOG_CORE
#  define LOG_CREATE_LAMBDA_OR_MACRO(name)                                                         \
  PR("\n\n[core " name "]");                                                                       \
  LOG(CAR(args), "fun args");                                                                      \
  LOG(CDR(args), "fun body")
#else
#  define LOG_CREATE_LAMBDA_OR_MACRO(name) ((void)0)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// math
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now. It mutates its first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_core_##name(ae_obj_t * const args) {                                                 \
  assert(CONSP(args));                                                                             \
                                                                                                   \
  ae_obj_t * accum = NIL;                                                                          \
  ae_obj_t * rest  = NIL;                                                                          \
                                                                                                   \
  if (NILP(CDR(args))) {                                                                           \
    accum = NEW_INT(default);                                                                      \
    rest = args;                                                                                   \
  }                                                                                                \
  else {                                                                                           \
    REQUIRE(args, INTEGERP(CAR(args)));                                                            \
                                                                                                   \
    accum = CAR(args);                                                                             \
    rest = CDR(args);                                                                              \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    REQUIRE(args, INTEGERP(elem));                                                                 \
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
  assert(CONSP(args));                                                                             \
                                                                                                   \
  bool result = init;                                                                              \
                                                                                                   \
  FOR_EACH(elem, args) {                                                                           \
    if (NILP(CDR(position)))                                                                       \
        break;                                                                                     \
                                                                                                   \
    REQUIRE(args, INTEGERP(elem));                                                                  \
    REQUIRE(args, INTEGERP(CADR(position)));                                                        \
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
  PR("\n\n[core setq]");
#endif

  int len = LENGTH(args);
  
  REQUIRE(env_and_args, SYMBOLP(CAR(args)));
  REQUIRE(env_and_args, len >= 1);
  REQUIRE(env_and_args, len <= 2); // CDR(args) is allowed to be NIL, so it may look like 1 arg.

  ae_obj_t * sym         = CAR(args);
  ae_obj_t * val         = CADR(args);

#ifdef AE_LOG_CORE
  LOG(sym, "core setq sym");
  LOG(val, "core setq val");
#endif

  val                    = EVAL(env, val);

  ENV_SET(env, sym, val);

  return val;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

#ifdef AE_LOG_CORE
  PR("\n\n[core progn]");
  LOG(env, "progn env");
  LOG(args, "progn args");
#endif

  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args)
    ret = EVAL(env, elem);

  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(ae_obj_t * const args) {
  REQUIRE(args, LENGTH(args) == 1);

  return PROPER_LISTP(CAR(args)) ? TRUE : NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  return FUN_PARAMS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  return FUN_BODY(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  REQUIRE(env_and_args, NILP(args));

  return env;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _parent
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_parent(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

  return ENVP(CAR(args))
    ? ENV_PARENT(CAR(args))
    : FUN_ENV(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _syms
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_syms(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && ENVP(CAR(args)));

  return ENV_SYMS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errmsg
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errmsg(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  return NEW_STRING(ERR_MSG(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errobj(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  return ERR_OBJ(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _vals
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_vals(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && ENVP(CAR(args)));

  return ENV_VALS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _numer
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_numer(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  return NEW_INT(NUMER_VAL(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _denom
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_denom(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  return NEW_INT((RATIONALP(CAR(args)))
                 ? DENOM_VAL(CAR(args))
                 : 1);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _type
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_type(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1));

  const char * type = TYPE_STR(CAR(args));
  /* */ char * tmp  = free_list_malloc(strlen(type) + 2);
  sprintf(tmp, ":%s", type);

  ae_obj_t   * sym  = SYM(tmp);

  free_list_free(tmp);

  return sym;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _quote
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_quote(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  REQUIRE(env_and_args, LENGTH(args) == 1);

  return CAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_exit(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && INTEGERP(CAR(args)));

  exit(INT_VAL(CAR(args)));

  return CAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eval(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  REQUIRE(env_and_args, LENGTH(args) == 1);

  return EVAL(env, EVAL(env, CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  LOG_CREATE_LAMBDA_OR_MACRO("lambda");

  
  REQUIRE(env_and_args,
          TAILP(CAR(args))
#ifndef AE_NO_SINGLE_SYM_PARAMS  
          || SYMBOLP(CAR(args))
#endif
          );
  REQUIRE(env_and_args, TAILP(CDR(args)));

  return NEW_LAMBDA(CAR(args), CDR(args), env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env_and_args) {
  SPECIAL_FUN_ARGS(env, args, env_and_args);

  LOG_CREATE_LAMBDA_OR_MACRO("macro");

  REQUIRE(env_and_args, TAILP(CAR(args)));
  REQUIRE(env_and_args, TAILP(CDR(args)));

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

  REQUIRE(env_and_args, CONSP(CAR(args)));

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

  if (! NILP(EVAL(env, caar)))
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
  PRINC(CONS(SYM("progn"), CDDR(args)));
#endif

  bool cond_result = ! NILP(EVAL(env, CAR(args)));

#ifdef AE_LOG_CORE
  PR("cond_result: ");
  PRINC(cond_result ? TRUE : NIL);
  NL;
#endif

  if (cond_result) {
#ifdef AE_LOG_CORE
    PR("Choose then.");
#endif
    return EVAL(env, CADR(args));
  }
  else {
#ifdef AE_LOG_CORE
    PR("Choose else.");
#endif
    return EVAL(env, CONS(SYM("progn"), CDDR(args)));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _msleep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_msleep(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && INTEGERP(CAR(args)));

  int ms = INT_VAL(CAR(args));

  usleep(ms * 1000);

  return CAR(args);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  int len = LENGTH(CAR(args));

  //OLOG(args);
  REQUIRE(CAR(args), ((void)"core length only works on proper lists!", len >= 0));
          
  return NEW_INT(len);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  return TAILP(CAR(args)) ? TRUE : NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  return NILP(CAR(args))
    ? NIL // car of nil is nil.
    : CAAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(ae_obj_t * const args) {
  REQUIRE(args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  return NILP(CAR(args))
    ? NIL // cdr of nil is nil.
    : CDAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(ae_obj_t * const args) {
  REQUIRE(args, LENGTH(args) == 2);

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);

  return NEW_CONS(head, tail);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(ae_obj_t * const args) {
  FOR_EACH(tailarg, CDR(args))
    if (NEQ(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(ae_obj_t * const args) {
  FOR_EACH(tailarg, CDR(args))
    if (NEQL(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _atomp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_atomp(ae_obj_t * const args) {
  REQUIRE(args, LENGTH(args) > 0);
  
  FOR_EACH(elem, args)
    if (! ATOMP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(ae_obj_t * const args) {
  FOR_EACH(elem, args)
    if (! NILP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ - on the current line, without quoting.
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(ae_obj_t * const args) {
  int written = 0;

  FOR_EACH(elem, args)
    written += PUT(elem);

  fflush(stdout);

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ - on the current line, without quoting.
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(ae_obj_t * const args) {
  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print - on a new line, with quoting. BUG: no quoting?
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(ae_obj_t * const args) {
  int written = 0;

  FOR_EACH(elem, args) {
    written += PRINT(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write - on the current line, with quoting. Works.
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_write(ae_obj_t * const args) {
  int written = 0;

  FOR_EACH(elem, args) {
    written += WRITE(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  return NEW_INT(written);
}
