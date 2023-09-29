#include <stdbool.h>
#include <unistd.h>

#include "ae_alist.h"
#include "ae_core.h"
#include "ae_env.h"
#include "ae_eval.h"
#include "ae_free_list.h"
#include "ae_list.h"
#include "ae_util.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define REQUIRE(cond, ...)                                                                         \
  if (! (cond)) {                                                                                  \
    char * fmt      = ("" __VA_ARGS__)[0]                                                          \
      ? "%s:%d: \"Error in %s: require " #cond ", " __VA_ARGS__ "!\""                              \
      : "%s:%d: \"Error in %s: require " #cond "!\"";                                              \
    char * msg      = free_list_malloc(256);                                                       \
    sprintf(msg, fmt, __FILE__, __LINE__, __func__);                                               \
                                                                                                   \
    ae_obj_t * err_data = NIL;                                                                     \
                                                                                                   \
    ASET(err_data, SYM("args"), args);                                                             \
    ASET(err_data, SYM("env"),  env);                                                              \
                                                                                                   \
    return NEW_ERROR(err_data, msg);                                                               \
  }

#ifdef AE_LOG_CORE
#  define LOG_CORE(name)                                                                           \
  PR("\n\n[core " name "]");                                                                       \
  LOG(args, name " args");                                                                         \
  LOG(env,  name " body")
#else
#  define LOG_CORE(name) ((void)0)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// math
////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS for now. It mutates its first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_core_##name(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {  \
  LOG_CORE(#name);                                                                                 \
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
    REQUIRE(INTEGERP(CAR(args)));                                                                  \
                                                                                                   \
    accum = CAR(args);                                                                             \
    rest = CDR(args);                                                                              \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    REQUIRE(INTEGERP(elem));                                                                       \
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
ae_obj_t * ae_core_##name(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {  \
  LOG_CORE(#name);                                                                                 \
  assert(CONSP(args));                                                                             \
                                                                                                   \
  bool result = init;                                                                              \
                                                                                                   \
  FOR_EACH(elem, args) {                                                                           \
    if (NILP(CDR(position)))                                                                       \
        break;                                                                                     \
                                                                                                   \
    REQUIRE(INTEGERP(elem));                                                                 \
    REQUIRE(INTEGERP(CADR(position)));                                                       \
                                                                                                   \
    result assign INT_VAL(elem) oper INT_VAL(CADR(position));                                      \
  }                                                                                                \
                                                                                                   \
  return TRUTH(result);                                                                            \
}

FOR_EACH_CMP_OP(DEF_CMP_OP);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aset(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("aset");
  
  int len = LENGTH(args);

  REQUIRE(len >= 2, "aset requires at least 2 args");
  REQUIRE(len <= 3, "aset requires 2 or 3 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);
  ae_obj_t * value = CADDR(args);

  REQUIRE(SYMBOLP(key));

  return ASET(alist, key, value);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _aget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_aget(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("aget");
  
  int len = LENGTH(args);
  
  REQUIRE(len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(SYMBOLP(key));

  return AGET(alist, key);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _ahas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_ahas(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("ahas");
  
  int len = LENGTH(args);

  REQUIRE(len == 2, "aget requires 2 args");

  ae_obj_t * alist = CAR(args);
  ae_obj_t * key   = CADR(args);

  REQUIRE(SYMBOLP(key));

  return TRUTH(AHAS(alist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("setq");

  int len = LENGTH(args);
  
  REQUIRE(len >= 1, "setq requires at least 1 arg");
  REQUIRE(len <= 2, "setq requires 1 or 2 args");
  
  ae_obj_t * sym         = CAR(args);
  ae_obj_t * val         = CADR(args);

  REQUIRE(SYMBOLP(sym));
  REQUIRE(sym != NIL,  "can't set nil");
  REQUIRE(sym != TRUE, "can't set t");
  
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

ae_obj_t * ae_core_progn(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("progn");
  
  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args)
    ret = EVAL(env, elem);

  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("properp");

  REQUIRE(LENGTH(args) == 1);

  return PROPER_LISTP(CAR(args)) ? TRUE : NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_params(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("params");

  REQUIRE((LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  return FUN_PARAMS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_body(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("body");

  REQUIRE((LENGTH(args) == 1) && (MACROP(CAR(args)) || LAMBDAP(CAR(args))));

  return FUN_BODY(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _env
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_env(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("env");

  int len = LENGTH(args);
  
  REQUIRE(len <= 1, "setq requires 0 or 1 args");

  if (len == 1) {
    REQUIRE((ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));
    
    return ENVP(CAR(args))
      ? ENV_PARENT(CAR(args))
      : FUN_ENV(CAR(args));
    
    /* REQUIRE(LAMBDAP(CAR(args)) || MACROP(CAR(args))); */

    /* return FUN_ENV(CAR(args)); */
  }
  
  return env;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _parent
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_parent(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("parent");

  REQUIRE(LENGTH(args) == 1);
  REQUIRE((ENVP(CAR(args)) || LAMBDAP(CAR(args)) || MACROP(CAR(args))));

  return ENVP(CAR(args))
    ? ENV_PARENT(CAR(args))
    : FUN_ENV(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _syms
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_syms(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("syms");

  REQUIRE((LENGTH(args) == 1) && ENVP(CAR(args)));

  return ENV_SYMS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errmsg
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errmsg(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("errmsg");

  REQUIRE((LENGTH(args) == 1) && ERRORP(CAR(args)));

  return NEW_STRING(ERR_MSG(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _errobj
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_errobj(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("errobj");

  REQUIRE((LENGTH(args) == 1) && ERRORP(CAR(args)));

  return ERR_OBJ(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _vals
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_vals(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("vals");

  REQUIRE((LENGTH(args) == 1) && ENVP(CAR(args)));

  return ENV_VALS(CAR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _numer
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_numer(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("numer");

  REQUIRE((LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  return NEW_INT(NUMER_VAL(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _denom
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_denom(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("denom");

  REQUIRE((LENGTH(args) == 1) && (RATIONALP(CAR(args)) || INTEGERP(CAR(args))));

  return NEW_INT((RATIONALP(CAR(args)))
                 ? DENOM_VAL(CAR(args))
                 : 1);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _type
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_type(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("type");

  REQUIRE((LENGTH(args) == 1));

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

ae_obj_t * ae_core_quote(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("quote");

  REQUIRE(LENGTH(args) == 1);

  return CAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_exit(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("exit");
  
  REQUIRE((LENGTH(args) == 1) && INTEGERP(CAR(args)));

  exit(INT_VAL(CAR(args)));

  return CAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eval(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("eval");
  
  REQUIRE(LENGTH(args) == 1);

  return EVAL(env, EVAL(env, CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_lambda(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("lambda");
  
  REQUIRE(TAILP(CAR(args))
#ifndef AE_NO_SINGLE_SYM_PARAMS  
          || SYMBOLP(CAR(args))
#endif
          );
  REQUIRE(TAILP(CDR(args)));

  return NEW_LAMBDA(CAR(args), CDR(args), env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_macro(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("macro");

  REQUIRE(TAILP(CAR(args)));
  REQUIRE(TAILP(CDR(args)));

  return NEW_MACRO(CAR(args), CDR(args), env);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cond
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cond(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("cond");

  REQUIRE("an empty cond does not make sense");

  ae_obj_t * caar = CAAR(args);
  ae_obj_t * cdar = CDAR(args);

  LOG(caar, "caar");
  LOG(cdar, "cdar");

  if (! NILP(EVAL(env, caar)))
    return EVAL(env, ae_core_progn(env, cdar));

  return ae_core_cond(env, CDR(args));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_if(ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("if");

#ifdef AE_LOG_CORE
  PR("if:          ");
  PRINC(CAR(args));
  NL;
  PR("then:        ");
  PRINC(CADR(args));
  NL;
  PR("else:        ");
  PRINC(CONS(SYM("progn"), CDDR(args)));
#endif

  REQUIRE(!NILP(CDR(args)), "if requires at least 2 args");
  
  bool cond_result = ! NILP(EVAL(env, CAR(args)));

#ifdef AE_LOG_CORE
  LOG(cond_result ? TRUE : NIL, "cond_result: ");
#endif

  if (cond_result) {
    PR("Choose then.\n");

    return EVAL(env, CADR(args));
  }
  else {
    PR("Choose else.\n");

    return EVAL(env, CONS(SYM("progn"), CDDR(args)));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _msleep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_msleep(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("msleep");

  REQUIRE((LENGTH(args) == 1) && INTEGERP(CAR(args)));

  int ms = INT_VAL(CAR(args));

  usleep(ms * 1000);

  return CAR(args);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _length
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_length(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("length");
  
  REQUIRE((LENGTH(args) == 1) && TAILP(CAR(args)));

  int len = LENGTH(CAR(args));

  REQUIRE(len >= 0, "core length only works on proper lists");
          
  return NEW_INT(len);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("tailp");

  REQUIRE((LENGTH(args) == 1) && TAILP(CAR(args)));

  return TAILP(CAR(args)) ? TRUE : NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_car(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("car");

  REQUIRE((LENGTH(args) == 1) && TAILP(CAR(args)));

  return NILP(CAR(args))
    ? NIL // car of nil is nil.
    : CAAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cdr(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("cdr");

  REQUIRE((LENGTH(args) == 1) && TAILP(CAR(args)));

  return NILP(CAR(args))
    ? NIL // cdr of nil is nil.
    : CDAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cons(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("cons");
  
  REQUIRE(LENGTH(args) == 2);

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);

  return NEW_CONS(head, tail);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("eq");

  FOR_EACH(tailarg, CDR(args))
    if (NEQ(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("eql");

  FOR_EACH(tailarg, CDR(args))
    if (NEQL(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _atomp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_atomp(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("atomp");

  REQUIRE(LENGTH(args) > 0);
  
  FOR_EACH(elem, args)
    if (! ATOMP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("not");

  FOR_EACH(elem, args)
    if (! NILP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("put");

  int written = 0;

  FOR_EACH(elem, args)
    written += PUT(elem);

  fflush(stdout);

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_princ(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("princ");

  int written = 0;

  FOR_EACH(elem, args)
    written += PRINC(elem);

  fflush(stdout);

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_print(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("print");

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
// _write
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_write(__attribute__ ((unused)) ae_obj_t * const env, ae_obj_t * const args) {
  LOG_CORE("write");

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
