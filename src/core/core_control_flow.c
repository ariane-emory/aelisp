#include "core_includes.h"
#include "eval.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("progn");

  ae_obj_t * ret = NIL;

  FOR_EACH(elem, args) {
    ret = EVAL(env, elem);
    if (ERRORP(ret))
      break;
  }

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
  CORE_BEGIN("if");

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

    CORE_RETURN("if", ae_eval(env, then_branch));
  }
  else {

#ifdef AE_LOG_CORE
    SLOG("chose else");
#endif

    CORE_RETURN("if", ae_core_progn(env, else_branch));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _when
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_when(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("when");

  ae_obj_t * const when_cond   = CAR(args);
  ae_obj_t * const then_branch = CDR(args);
  
#ifdef AE_LOG_CORE
  LOG(when_cond,   "when");
  LOG(then_branch, "then");
#endif

  REQUIRE(env, args, !NILP(CDR(args)), "when requires at least 2 args");

  bool cond_result = ! NILP(EVAL(env, when_cond));

#ifdef AE_LOG_CORE
  LOG(cond_result ? TRUE : NIL, "cond_result: ");
#endif

  if (cond_result) {

#ifdef AE_LOG_CORE
    SLOG("chose when");
#endif

    CORE_RETURN("when", ae_core_progn(env, then_branch));
  }

#ifdef AE_LOG_CORE
  SLOG("chose nil");
#endif
  
  CORE_RETURN("when", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _or
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_or(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("or");

  ae_obj_t * const either_branch = CAR(args);
  ae_obj_t * const or_branch     = CADR(args);
  
#ifdef AE_LOG_CORE
  LOG(either_branch, "either");
  LOG(or_branch,     "or");
#endif

  REQUIRE(env, args, !NILP(CDR(args)), "or requires 2 args");
 
  ae_obj_t * either_result = EVAL(env, either_branch);

#ifdef AE_LOG_CORE
  LOG(either_result, "either_result: ");
#endif

  if (! NILP(either_result)) {

#ifdef AE_LOG_CORE
    SLOG("chose either");
#endif

    CORE_RETURN("or", either_result);
  }
  else {

#ifdef AE_LOG_CORE
    SLOG("chose or");
#endif

    CORE_RETURN("or", ae_eval(env, or_branch));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _and
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_and(ae_obj_t * const env, ae_obj_t * const args) {
  CORE_BEGIN("and");

  ae_obj_t * const this_branch = CAR(args);
  ae_obj_t * const and_that_branch     = CADR(args);
  
#ifdef AE_LOG_CORE
  LOG(this_branch,     "this");
  LOG(and_that_branch, "and_that");
#endif

  REQUIRE(env, args, !NILP(CDR(args)), "and requires 2 args");
 
  ae_obj_t * this_result = EVAL(env, this_branch);
  
#ifdef AE_LOG_CORE
  LOG(this_result, "this_result: ");
#endif
  
  if (NILP(this_result)) {

#ifdef AE_LOG_CORE
    SLOG("not this");
#endif

    CORE_RETURN("and", NIL);
  }

#ifdef AE_LOG_CORE
    SLOG("this and");
#endif

    CORE_RETURN("and", ae_eval(env, and_that_branch));
}

