#include "core_includes.h"
#include "eval.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("progn");

  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args)
    ret = elem; // EVAL(env, elem);

  CORE_RETURN("progn", ret);
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

  CORE_RETURN("cond", ae_core_cond(env, CDR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// old _cond
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cond2(ae_obj_t * const env, ae_obj_t * const args) {
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

  CORE_RETURN("cond", ae_core_cond2(env, CDR(args)));
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

