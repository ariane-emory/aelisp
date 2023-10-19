#include "core_includes.h"

#define BAIL_IF_ERROR(obj)                                                                         \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (ERRORP(CAPTURED)) {                                                                        \
      ret = CAPTURED;                                                                              \
      goto end;                                                                                    \
    }                                                                                              \
                                                                                                   \
    CAPTURED;                                                                                      \
  })

#define RETURN(obj)                                                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    ret = CAPTURED;                                                                                \
    goto end;                                                                                      \
    (void)CAPTURED;                                                                                \
  })

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

    ret = EVAL(env, elem);
    
    BAIL_IF_ERROR(ret);
        
    if (log_core)
      LOG(ret, "progn arg #%d/%d evaluated to", ctr, args_length);
  }

end:
  
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
  ae_obj_t *       ret         = NIL; 
  
  if (log_core) {
    LOG(when_cond,   "when");
    LOG(then_branch, "then");
  }

  bool cond_result = ! NILP(BAIL_IF_ERROR(EVAL(env, when_cond)));

  if (log_core)
    LOG(cond_result ? TRUE : NIL, "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");

    RETURN(BAIL_IF_ERROR(ae_core_progn(env, then_branch, LENGTH(then_branch))));
  }

  if (log_core)
    SLOG("chose nil");

end:
  
  CORE_RETURN("when", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _unless
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_unless(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("unless");

  ae_obj_t * const unless_cond = CAR(args);
  ae_obj_t * const then_branch = CDR(args);
  ae_obj_t *       ret         = NIL;
  
  if (log_core) {
    LOG(unless_cond, "unless");
    LOG(then_branch, "then");
  }

  bool cond_result = NILP(BAIL_IF_ERROR(EVAL(env, unless_cond)));

  if (log_core)
    LOG(cond_result ? TRUE : NIL, "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");

    RETURN(BAIL_IF_ERROR(ae_core_progn(env, then_branch, LENGTH(then_branch))));
  }

  if (log_core)
    SLOG("chose nil");

end:
  
  CORE_RETURN("unless", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _or
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_or(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("or");

  ae_obj_t * ret = NIL;
  
  FOR_EACH(option, args) {
    ret = BAIL_IF_ERROR(EVAL(env, option));
    
    if (! NILP(ret))
      RETURN(ret);
  }
      
end:
    
  CORE_RETURN("or", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _and
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_and(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("and");

  ae_obj_t * ret = NIL;
  
  FOR_EACH(option, args) {
    ret = BAIL_IF_ERROR(EVAL(env, option));
    
    if (NILP(ret))
      RETURN(NIL);
  }

end:
  
  CORE_RETURN("and", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _while
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_while(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("while");

  ae_obj_t * const while_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  ae_obj_t *       ret        = NIL;
  
  if (log_core) {
    LOG(while_cond, "while");
    LOG(do_branch,  "do");
  }
  
  while (! NILP(BAIL_IF_ERROR(EVAL(env, while_cond)))) {
    if (log_core)
      LOG(do_branch, "do while");

    BAIL_IF_ERROR(ae_core_progn(env, do_branch, LENGTH(do_branch)));
  }

end:
  
  if (log_core)
    SLOG("left while");
  
  CORE_RETURN("while", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _until
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_until(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("until");

  ae_obj_t * const until_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  ae_obj_t *       ret        = NIL;
  
  if (log_core) {
    LOG(until_cond, "until");
    LOG(do_branch,  "do");
  }
  
  // ae_obj_t * cond_result = NIL;

  while (NILP(BAIL_IF_ERROR(EVAL(env, until_cond)))) {
    if (log_core)
      LOG(do_branch, "do until");

    BAIL_IF_ERROR(ae_core_progn(env, do_branch, LENGTH(do_branch)));
  }

end:
  
  if (log_core)
    SLOG("left until");
  
  CORE_RETURN("until", NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _repeat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_repeat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("repeat");

  ae_obj_t * ret       = NIL;
  ae_obj_t * first_arg = BAIL_IF_ERROR(EVAL(env, CAR(args)));
 
  REQUIRE(env, args, INTEGERP(first_arg), "repeat requires an integer as its first argument");

  long long int times = INT_VAL(first_arg);

  for (long long int ix = 0; ix < times; ix++)
    BAIL_IF_ERROR(ae_core_progn(env, CDR(args), LENGTH(CDR(args))));

end:
  
  CORE_RETURN("repeat", NIL);
}

