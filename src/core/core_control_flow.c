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

  int ctr = 0;
  
  FOR_EACH(elem, args) {
    ctr++;

    if (log_core)
      LOG(elem, "eval progn arg  #%d/%d", ctr, args_length);

    // INDENT;

    ret = EVAL(env, elem);
    
    if (ERRORP(ret))
      break;

    // OUTDENT;
    
    if (log_core)
      LOG(ret, "progn arg #%d/%d evaluated to", ctr, args_length);
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
  
    if (log_core) {
      LOG(item_car, "cond item's car");
      LOG(item_cdr, "cond item's cdr");
    }

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
      
  if (log_core) {
    LOG(if_cond,     "if");
    LOG(then_branch, "then");
    LOG(else_branch, "else");
  }

  bool cond_result = ! NILP(EVAL(env, if_cond));

  if (log_core) 
    LOG(cond_result ? TRUE : NIL, "cond_result: ");

  if (cond_result) {

    if (log_core)
      LOG(then_branch, "chose then");

    CORE_RETURN("if", ae_eval(env, then_branch));
  }
  else {

    if (log_core)
      LOG(else_branch, "chose else");

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
  
  if (log_core) {
    LOG(when_cond,   "when");
    LOG(then_branch, "then");
  }

  bool cond_result = ! NILP(EVAL(env, when_cond));

  if (log_core)
    LOG(cond_result ? TRUE : NIL, "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");


    CORE_RETURN("when", ae_core_progn(env, then_branch, LENGTH(then_branch)));
  }

  if (log_core)
    SLOG("chose nil");
  
  CORE_RETURN("when", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _or
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_or(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("or");

  ae_obj_t * const either_branch = CAR(args);
  ae_obj_t * const or_branch     = CADR(args);
  
  if (log_core) {
    LOG(either_branch, "either");
    LOG(or_branch,     "or");
  }

  ae_obj_t * either_result = EVAL(env, either_branch);

  if (log_core)
    LOG(either_result, "either_result: ");

  if (! NILP(either_result)) {

    if (log_core)
      LOG(either_branch, "chose either");

    CORE_RETURN("or", either_result);
  }
  else {
    if (log_core)
      LOG(or_branch, "chose or");

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
  
  if (log_core) {
    LOG(this_branch,     "this");
    LOG(and_that_branch, "and_that");
  }

  REQUIRE(env, args, LENGTH(args) == 2, "and requires 2 args");
 
  ae_obj_t * this_result = EVAL(env, this_branch);
  
  if (log_core)
    LOG(this_result, "this_result: ");
  
  if (NILP(this_result)) {

    if (log_core)
      SLOG("not this");

    CORE_RETURN("and", NIL);
  }

  if (log_core)
    SLOG("this and");

  CORE_RETURN("and", ae_eval(env, and_that_branch));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _when
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_while(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("while");

  ae_obj_t * const while_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  
  if (log_core) {
    LOG(while_cond, "while");
    LOG(do_branch,  "do");
  }
  
  ae_obj_t * cond_result = NIL;
  
  while (!NILP(cond_result = EVAL(env, while_cond))) {
  
    if (log_core)
      LOG(do_branch, "do while");

    ae_core_progn(env, do_branch, LENGTH(do_branch));
  }

  if (log_core)
    SLOG("left while");
  
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


