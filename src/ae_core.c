#include <stdbool.h>
#include <unistd.h>

#include "ae_alist.h"
#include "ae_core.h"
#include "ae_env.h"
#include "ae_eval.h"
#include "ae_free_list.h"
#include "ae_list.h"
#include "ae_obj.h"
#include "ae_util.h"
#include "ae_write.h"
#include "require.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef AE_LOG_CORE
#  define CORE_BEGIN(name)                                                                         \
({                                                                                                 \
  LOG(args, "[core_" name "]");                                                                    \
  INDENT;                                                                                          \
  LOG(env,  "in env");                                                                             \
})
#else
#  define CORE_BEGIN(name) ((void)name)
#endif

#ifdef AE_LOG_CORE
#  define CORE_RETURN(name, val)                                                                   \
({                                                                                                 \
 OUTDENT;                                                                                          \
 LOG_RETURN_WITH_TYPE("core_" name, val);                                                          \
 return val;                                                                                       \
})
#else
#  define CORE_RETURN(name, val) return ((val))
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// math
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now. It mutates its first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_core_ ## name(ae_obj_t * const env, ae_obj_t * const args) {                         \
  CORE_BEGIN(#name);                                                                               \
  assert(CONSP(args));                                                                             \
                                                                                                   \
  int        accum = 0;                                                                            \
  ae_obj_t * rest  = NIL;                                                                          \
                                                                                                   \
  if (NILP(CDR(args))) {                                                                           \
    accum = default;                                                                               \
    rest  = args;                                                                                  \
  }                                                                                                \
  else {                                                                                           \
    REQUIRE(env, args, INTEGERP(CAR(args)));                                                       \
                                                                                                   \
    accum = INT_VAL(CAR(args));                                                                    \
    rest  = CDR(args);                                                                             \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    REQUIRE(env, args, INTEGERP(elem));                                                            \
    accum = accum oper INT_VAL(elem);                                                              \
  }                                                                                                \
                                                                                                   \
  CORE_RETURN(#name, NEW_INT(accum));                                                              \
}

FOR_EACH_CORE_MATH_OP(DEF_MATH_OP);

////////////////////////////////////////////////////////////////////////////////////////////////////
// numeric comparison
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

FOR_EACH_CORE_CMP_OP(DEF_CMP_OP);

 ///////////////////////////////////////////////////////////////////////////////////////////////////
// _aset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aset(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("aset");

  int len = LENGTH(args);

  REQUIRE(env, args, len >= 2, "aset requires at least 2 args");
  REQUIRE(env, args, len <= 3, "aset requires 2 or 3 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);
  ae_obj_t * value = CADDR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("aset", ASET(alist, key, value));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aget(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("aget");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("aget", AGET(alist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _ahas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_ahas(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("ahas");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("ahas", TRUTH(AHAS(alist, key)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pset(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("pset");

  int len = LENGTH(args);

  REQUIRE(env, args, len >= 2, "pset requires at least 2 args");
  REQUIRE(env, args, len <= 3, "pset requires 2 or 3 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);
  ae_obj_t * value = CADDR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("pset", PSET(plist, key, value));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _pget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_pget(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("pget");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "pget requires 2 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("pget", PGET(plist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _phas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_phas(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("phas");

  int len = LENGTH(args);

  REQUIRE(env, args, len == 2, "pget requires 2 args");

  ae_obj_t * plist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(env, args, SYMBOLP(key));

  CORE_RETURN("phas", TRUTH(PHAS(plist, key)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env, ae_obj_t * const args) {
#ifdef AE_LOG_CORE
  // No CORE_BEGIN!
  LOG(args, "[core_setq]");
  INDENT;
#endif

  int len = LENGTH(args);

  REQUIRE(env, args, len >= 1, "setq requires at least 1 arg");
  REQUIRE(env, args, len <= 2, "setq requires 1 or 2 args");

  ae_obj_t * sym         = CAR(args);
  ae_obj_t * val         = CADR(args);

  REQUIRE(env, args, SYMBOLP(sym));
  REQUIRE(env, args, ! KEYWORDP(sym), "keyword symbols are constant");
  REQUIRE(env, args, sym != NIL,  "nil is a constant symbol");
  REQUIRE(env, args, sym != TRUE, "t is a constant symbol");

#ifdef AE_LOG_CORE
  LOG(sym, "setting symbol");
  LOG(val, "to value");
#endif

  ae_obj_t * setq_env = env;
  
#ifdef AE_CORE_ENVS
  if (ENV_PARENT(env) != NIL)
    setq_env = ENV_PARENT(env);
#endif

#ifdef AE_LOG_CORE
  LOG(val, "evaluating 'value' argument");
  INDENT;
#endif

  val = EVAL(setq_env, val);

#ifdef AE_LOG_CORE
  OUTDENT;
  LOG(val, "evaluated 'value' argument is");
#endif

#ifdef AE_DEBUG_OBJ
  if (LAMBDAP(val) || MACROP(val)) {
    DSET(val, "last-bound-to", sym);

#  ifdef AE_LOG_CORE
    LOG(DOBJ(val), "core setq val's new debug data");
#  endif

  }
#endif

  ENV_SET(setq_env, sym, val);

  CORE_RETURN("setq", val);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("progn");

  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args)
    ret = EVAL(env, elem);

  CORE_RETURN("progn", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("properp");

  REQUIRE(env, args, LENGTH(args) == 1);

  CORE_RETURN("properp", PROPER_LISTP(CAR(args)) ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("params");

  REQUIRE(env, args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  CORE_RETURN("params", FUN_PARAMS(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("body");

  REQUIRE(env, args, (LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  CORE_RETURN("body", FUN_BODY(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nl
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nl(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("nl");

  int len = LENGTH(args);

  REQUIRE(env, args, len = 1, "nl takes no args");

  NL;

  CORE_RETURN("nl", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("env");

  int len = LENGTH(args);

  REQUIRE(env, args, len <= 1, "env requires 0 or 1 args");

  if (len == 1) {
    REQUIRE(env, args, (ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

    CORE_RETURN("env", ENVP(CAR(args))
                ? ENV_PARENT(CAR(args))
                : FUN_ENV(CAR(args)));
  }

  CORE_RETURN("env", env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _syms
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_syms(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("syms");

  REQUIRE(env, args, (LENGTH(args) == 1) && ENVP(CAR(args)));

  CORE_RETURN("syms", ENV_SYMS(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dobj(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("dobj");

  REQUIRE(env, args, (LENGTH(args) == 1));

#ifdef AE_DEBUG_OBJ
  CORE_RETURN("dobj", DOBJ(CAR(args)));
#else
  CORE_RETURN("dobj", NIL);
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errmsg
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errmsg(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("errmsg");

  REQUIRE(env, args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  CORE_RETURN("errmsg", NEW_STRING(EMSG(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errobj(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("errobj");

  REQUIRE(env, args, (LENGTH(args) == 1) && ERRORP(CAR(args)));

  CORE_RETURN("errobj", EOBJ(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _vals
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_vals(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("vals");

  REQUIRE(env, args, (LENGTH(args) == 1) && ENVP(CAR(args)));

  CORE_RETURN("vals", ENV_VALS(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _numer
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_numer(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("numer");

  REQUIRE(env, args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  CORE_RETURN("numer", NEW_INT(NUMER_VAL(CAR(args))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _denom
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_denom(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("denom");

  REQUIRE(env, args, (LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  CORE_RETURN("denom", NEW_INT((RATIONALP(CAR(args)))
                               ? DENOM_VAL(CAR(args))
                               : 1));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _type
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_type(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("type");

  REQUIRE(env, args, (LENGTH(args) == 1));

  const char * type = GET_TYPE_STR(CAR(args));
  /* */ char * tmp  = free_list_malloc(strlen(type) + 2);

  sprintf(tmp, ":%s", type);

  ae_obj_t   * sym  = SYM(tmp);

  free_list_free(tmp);

  CORE_RETURN("type", sym);
}

/* //////////////////////////////////////////////////////////////////////////////////////////////////// */
/* // _quote */
/* //////////////////////////////////////////////////////////////////////////////////////////////////// */

/* ae_obj_t * ae_core_quote(ae_obj_t * const env, ae_obj_t * const args) { */
/*   CORE_BEGIN("quote"); */

/*   REQUIRE(env, args, LENGTH(args) == 1); */

/*   CORE_RETURN("quote", CAR(args)); */
/* } */

////////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_exit(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("exit");

  REQUIRE(env, args, (LENGTH(args) == 1) && INTEGERP(CAR(args)));

  exit(INT_VAL(CAR(args)));

  CORE_RETURN("exit", CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eval(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("eval");

  REQUIRE(env, args, LENGTH(args) == 1);

  ae_obj_t * ret = EVAL(env, EVAL(env, CAR(args)));

  CORE_RETURN("eval", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("lambda");

  REQUIRE(env, args, TAILP(CAR(args))
#ifndef AE_NO_SINGLE_SYM_PARAMS
          || SYMBOLP(CAR(args))
#endif
          );
  REQUIRE(env, args, TAILP(CDR(args)));

  CORE_RETURN("lambda", NEW_LAMBDA(CAR(args), CDR(args), env));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("macro");

  REQUIRE(env, args, TAILP(CAR(args)));
  REQUIRE(env, args, TAILP(CDR(args)));

  CORE_RETURN("macro", NEW_MACRO(CAR(args), CDR(args), env));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cond
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cond(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("cond");

  REQUIRE(env, args, "an empty cond does not make sense");

  ae_obj_t * caar = CAAR(args);
  ae_obj_t * cdar = CDAR(args);

#ifdef AE_LOG_CORE
  LOG(caar, "caar");
  LOG(cdar, "cdar");
#endif

  if (! NILP(EVAL(env, caar)))
    CORE_RETURN("cond", EVAL(env, ae_core_progn(env, cdar)));

  CORE_RETURN("cpmd", ae_core_cond(env, CDR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_if(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("if");

#ifdef AE_LOG_CORE
  LOG(CAR(args),                      "if");
  LOG(CADR(args),                     "then");
  LOG(CONS(SYM("progn"), CDDR(args)), "else");
#endif

  REQUIRE(env, args, !NILP(CDR(args)), "if requires at least 2 args");

  bool cond_result = ! NILP(EVAL(env, CAR(args)));

#ifdef AE_LOG_CORE
  LOG(cond_result ? TRUE : NIL, "cond_result: ");
#endif

  if (cond_result) {

#ifdef AE_LOG_CORE
    SLOG("chose then");
#endif

    CORE_RETURN("if", EVAL(env, CADR(args)));
  }
  else {

#ifdef AE_LOG_CORE
    SLOG("chose else");
#endif

    CORE_RETURN("if", EVAL(env, CONS(SYM("progn"), CDDR(args))));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _msleep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_msleep(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("msleep");

  REQUIRE(env, args, (LENGTH(args) == 1) && INTEGERP(CAR(args)));

  int ms = INT_VAL(CAR(args));

  usleep(ms * 1000);

  CORE_RETURN("msleep", CAR(args));
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("length");

  REQUIRE(env, args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  int len = LENGTH(CAR(args));

  REQUIRE(env, args, len >= 0, "core length only works on proper lists");

  CORE_RETURN("length", NEW_INT(len));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("tailp");

  REQUIRE(env, args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  CORE_RETURN("tailp", TAILP(CAR(args)) ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("car");

  REQUIRE(env, args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  CORE_RETURN("car", NILP(CAR(args))
              ? NIL // car of nil is nil.
              : CAAR(args));
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("cdr");

  REQUIRE(env, args, (LENGTH(args) == 1) && TAILP(CAR(args)));

  CORE_RETURN("cdr", NILP(CAR(args))
              ? NIL // cdr of nil is nil.
              : CDAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplaca
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplaca(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("rplaca");

  REQUIRE(env, args, (LENGTH(args) <= 2) && CONSP(CAR(args)));

  CAAR(args) = CADR(args);

  CORE_RETURN("rplaca", CADR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rplacd
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_rplacd(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("rplacd");

  REQUIRE(env, args, (LENGTH(args) <= 2) && CONSP(CAR(args)));

  CDAR(args) = CADR(args);

  CORE_RETURN("rplacd", CADR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("cons");

  REQUIRE(env, args, LENGTH(args) >= 1);
  REQUIRE(env, args, LENGTH(args) <= 2);

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);

  CORE_RETURN("cons", NEW_CONS(head, tail));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("eq");

  FOR_EACH(tailarg, CDR(args))
    if (NEQ(CAR(args), tailarg))
      CORE_RETURN("eq", NIL);

  CORE_RETURN("eq", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("eql");

  FOR_EACH(tailarg, CDR(args))
    if (NEQL(CAR(args), tailarg))
      CORE_RETURN("eql", NIL);

  CORE_RETURN("eql", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("not");

  FOR_EACH(elem, args)
    if (! NILP(elem))
      CORE_RETURN("not", NIL);

  CORE_RETURN("not", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("put");

  int written = 0;

  FOR_EACH(elem, args)
    written += PUT(elem);

  fflush(stdout);

  CORE_RETURN("put", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("princ");

  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  CORE_RETURN("princ", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("print");

  int written = 0;

  FOR_EACH(elem, args) {
    written += PRINT(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  CORE_RETURN("print", NEW_INT(written));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_write(__attribute__((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("write");

  int written = 0;

  FOR_EACH(elem, args) {
    written += WRITE(elem);

    if (! NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  fflush(stdout);

  CORE_RETURN("write", NEW_INT(written));
}
