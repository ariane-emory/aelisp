#include "core_includes.h"
#include "eval.h"

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
// _cond
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_cond(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("cond");

  REQUIRE(env, args, LENGTH(args) > 0, "an empty cond does not make sense");

  ae_obj_t * ret = NIL;
  
  FOR_EACH(cond_item, args) {
    REQUIRE(env, args, PROPERP(cond_item) && LENGTH(cond_item) > 1, "cond arguments must be proper lists with at least two elements");

    ae_obj_t * const item_car = CAR(cond_item);
    ae_obj_t * const item_cdr = CDR(cond_item);
  
#ifdef AE_LOG_CORE
    LOG(item_car, "car");
    LOG(item_cdr, "cdr");
#endif

    if (! NILP(EVAL(env, item_car))) {
      ret = ae_core_progn(env, item_cdr);

      break;
    }
  }

  CORE_RETURN("cond", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_if(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("iff");

  ae_obj_t * const if_cond     = CAR(args);
  ae_obj_t * const then_branch = CADR(args);
  ae_obj_t * const else_branch = CDDR(args);
    
  
#ifdef AE_LOG_CORE
  LOG(if_cond,     "if");
  LOG(then_branch, "then");
  LOG(else_branch, "else");
#endif

  REQUIRE(env, args, !NILP(CDR(args)), "if requires at least 2 args");

  bool cond_result = ! NILP(EVAL(env, if_cond));

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

    CORE_RETURN("if", ae_core_progn(env, CDDR(args)));
  }
}

