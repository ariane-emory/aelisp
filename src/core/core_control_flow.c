#include "core_includes.h"
#include "eval.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _repeat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_repeat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("repeat");

  ae_obj_t * first_arg = EVAL(env, CAR(args));

  REQUIRE(env, args, INTEGERP(first_arg), "repeat requires an integer as its first argument");

  long long int times = INT_VAL(first_arg);

  for (long long int ix = 0; ix < times; ix++)
    ae_core_progn(env, CDR(args), LENGTH(CDR(args)));

  CORE_RETURN("repeat", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_progn(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
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

ae_obj_t * ae_core_cond(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("cond");

  ae_obj_t * ret = NIL;
  
  FOR_EACH(cond_item, args) {
    REQUIRE(env, args, PROPERP(cond_item) && LENGTH(cond_item) > 1, "cond arguments must be proper lists with at least two elements");

    ae_obj_t * const item_car = CAR(cond_item);
    ae_obj_t * const item_cdr = CDR(cond_item);
  
#ifdef AE_LOG_CORE
    LOG(item_car, "cond item's car");
    LOG(item_cdr, "cond item's cdr"); 
#endif

    if (! NILP(EVAL(env, item_car))) {
      ret = ae_core_progn(env, item_cdr, LENGTH(item_cdr));

      break;
    }
  }

  CORE_RETURN("cond", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_if(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("if");

  ae_obj_t * const if_cond     = CAR(args);
  ae_obj_t * const then_branch = CADR(args);
  ae_obj_t * const else_branch = CDDR(args);
      
#ifdef AE_LOG_CORE
  LOG(if_cond,     "if");
  LOG(then_branch, "then");
  LOG(else_branch, "else");
#endif

  bool cond_result = ! NILP(EVAL(env, if_cond));

#ifdef AE_LOG_CORE
  LOG(cond_result ? TRUE : NIL, "cond_result: ");
#endif

  if (cond_result) {

#ifdef AE_LOG_CORE
    LOG(then_branch, "chose then");
#endif

    CORE_RETURN("if", ae_eval(env, then_branch));
  }
  else {

#ifdef AE_LOG_CORE
    LOG(else_branch, "chose else");
#endif

    CORE_RETURN("if", ae_core_progn(env, else_branch, LENGTH(else_branch)));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _when
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_when(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("when");

  ae_obj_t * const when_cond   = CAR(args);
  ae_obj_t * const then_branch = CDR(args);
  
#ifdef AE_LOG_CORE
  LOG(when_cond,   "when");
  LOG(then_branch, "then");
#endif

  bool cond_result = ! NILP(EVAL(env, when_cond));

#ifdef AE_LOG_CORE
  LOG(cond_result ? TRUE : NIL, "cond_result: ");
#endif

  if (cond_result) {

#ifdef AE_LOG_CORE
    LOG(then_branch, "chose then");
#endif

    CORE_RETURN("when", ae_core_progn(env, then_branch, LENGTH(then_branch)));
  }

#ifdef AE_LOG_CORE
  SLOG("chose nil");
#endif
  
  CORE_RETURN("when", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _or
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_or(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("or");

  ae_obj_t * const either_branch = CAR(args);
  ae_obj_t * const or_branch     = CADR(args);
  
#ifdef AE_LOG_CORE
  LOG(either_branch, "either");
  LOG(or_branch,     "or");
#endif

  ae_obj_t * either_result = EVAL(env, either_branch);

#ifdef AE_LOG_CORE
  LOG(either_result, "either_result: ");
#endif

  if (! NILP(either_result)) {

#ifdef AE_LOG_CORE
    LOG(either_branch, "chose either");
#endif

    CORE_RETURN("or", either_result);
  }
  else {

#ifdef AE_LOG_CORE
    LOG(or_branch, "chose or");
#endif

    CORE_RETURN("or", ae_eval(env, or_branch));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _and
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_and(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("and");

  ae_obj_t * const this_branch     = CAR(args);
  ae_obj_t * const and_that_branch = CADR(args);
  
#ifdef AE_LOG_CORE
  LOG(this_branch,     "this");
  LOG(and_that_branch, "and_that");
#endif

  REQUIRE(env, args, LENGTH(args) == 2, "and requires 2 args");
 
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

////////////////////////////////////////////////////////////////////////////////////////////////////
// _when
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_while(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("while");

  ae_obj_t * const while_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  
#ifdef AE_LOG_CORE
  LOG(while_cond, "while");
  LOG(do_branch,  "do");
#endif
  
  ae_obj_t * cond_result = NIL;
  
  while (!NILP(cond_result = EVAL(env, while_cond))) {
  
#ifdef AE_LOG_CORE
    LOG(do_branch, "do while");
#endif

    ae_core_progn(env, do_branch, LENGTH(do_branch));
  }

#ifdef AE_LOG_CORE
  SLOG("left while");
#endif
  
  CORE_RETURN("while", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_apply(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("apply");

  ae_obj_t * const func = CAR(args);
  ae_obj_t * const func_args = CADR(args);

  REQUIRE(env, args, PROPERP(func_args), "apply requires a proper list as its second argument");

  ae_obj_t * const call_expr = CONS(func, func_args);

  CORE_RETURN("apply", EVAL(env, call_expr));
}


