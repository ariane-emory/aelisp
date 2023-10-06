#include <stdbool.h>
#include <unistd.h>

#include "alist.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "obj.h"
#include "util.h"
#include "write.h"
#include "core_util_macros.h"

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
// _list
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_list(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("list");

  ae_obj_t * ret = args;

  CORE_RETURN("list", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _quote
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_quote(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("quote");

  REQUIRE(env, args, LENGTH(args) == 1);

  ae_obj_t * ret = CAR(args);

  CORE_RETURN("quote", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eval(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("eval");

  REQUIRE(env, args, LENGTH(args) == 1);

  ae_obj_t * ret = EVAL(env, CAR(args));

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

