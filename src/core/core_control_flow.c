#include "core_includes.h"

#include "jump_return.h"
#include "common.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _progn
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(progn) {
  int ctr = 0;
  
  FOR_EACH(elem, args) {
    ctr++;

    if (log_core)
      LOG(elem, "eval progn arg  #%d/%d", ctr, args_length);

    ret = RETURN_IF_ERRORP(EVAL(env, elem));
        
    if (log_core)
      LOG(ret, "progn arg #%d/%d evaluated to", ctr, args_length);
  }

  END_DEF_CORE_FUN(progn);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _case
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(case) {
  ae_obj_t * const key_form   = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ae_obj_t * const case_forms = CDR(args);

  REQUIRE(! NILP(case_forms),
          "case requires at least one form after the key form");

  if (log_core) {
    LOG(key_form,   "key_form");
    LOG(case_forms, "case_forms");
  }

  bool else_found = false;

  // first pass: Check for well-formedness and multiple else clauses
  FOR_EACH(case_form, case_forms) {
    REQUIRE(PROPERP(case_form) && LENGTH(case_form) > 1,
            "case forms must be proper lists with at least two elements");

    ae_obj_t * const case_form_car = CAR(case_form);

    if (case_form_car == SYM("else")) {
      REQUIRE(!else_found,
              "Only one else clause is allowed in a case expression");

      else_found = true;
    }
  }

  // second pass: evaluate which case matches
  FOR_EACH(case_form, case_forms) {
    ae_obj_t * const case_form_car = CAR(case_form);
    ae_obj_t * const case_form_cdr = CDR(case_form);

    if (log_core) {
      LOG(case_form_car, "case_form's car");
      LOG(case_form_cdr, "case_form's cdr");
    }

    if (case_form_car == SYM("else")) {
      RETURN(ae_core_progn(env, case_form_cdr, LENGTH(case_form_cdr)));
    } else {
      ae_obj_t * elements_to_check = CONSP(case_form_car)
        ? case_form_car 
        : CONS(case_form_car, NIL); // single element (atom) case form

      FOR_EACH(case_form_car_elem, elements_to_check) {
        if (log_core)
          LOG(case_form_car_elem, "case_form_car_elem");

        if (EQL(key_form, case_form_car_elem)) {
          if (log_core)
            SLOG("matches");
        
          RETURN(ae_core_progn(env, case_form_cdr, LENGTH(case_form_cdr)));
        } else if (log_core) {
          SLOG("doesn't match");
        }
      }
    }
  }

  END_DEF_CORE_FUN(case);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _cond
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(cond) {
  bool else_found = false;
    
  // first pass: Ensure no duplicate 'else' clauses and validate structure
  FOR_EACH(cond_item, args) {
    REQUIRE(PROPERP(cond_item) && LENGTH(cond_item) > 1,
            "cond arguments must be proper lists with at least two elements");

    ae_obj_t * const item_car = CAR(cond_item);

    if (item_car == SYM("else")) {
      REQUIRE(!else_found,
              "Only one else clause is allowed in a cond expression");

      else_found = true;

      REQUIRE(NILP(CDR(position)),
              "If used, else clause must be the last clause in a cond expression");
    }
  }

  // second pass: evaluate the conditions
  FOR_EACH(cond_item, args) {
    ae_obj_t * const item_car         = CAR(cond_item);
    ae_obj_t * const item_cdr         = CDR(cond_item);
    ae_obj_t *       cond_test_result = NIL;

    cond_test_result = item_car == SYM("else")
      ? TRUE
      : RETURN_IF_ERRORP(EVAL(env, item_car));

    if (! NILP(cond_test_result))
      RETURN(ae_core_progn(env, item_cdr, LENGTH(item_cdr)));
  }

  END_DEF_CORE_FUN(cond);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _if
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(if) {
  ae_obj_t * const if_cond     = CAR(args);
  ae_obj_t * const then_branch = CADR(args);
  ae_obj_t * const else_branch = CDDR(args);
  
  if (log_core) {
    LOG(if_cond,     "if");
    LOG(then_branch, "then");
    LOG(else_branch, "else");
  }

  bool cond_result = ! NILP(RETURN_IF_ERRORP(EVAL(env, if_cond)));

  if (log_core) 
    LOG(TRUTH(cond_result), "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");

    RETURN(EVAL(env, then_branch));
  } 
  else {
    if (log_core)
      LOG(else_branch, "chose else");

    RETURN(ae_core_progn(env, else_branch, LENGTH(else_branch)));
  }

  END_DEF_CORE_FUN(if);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _when
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(when) {
  ae_obj_t * const when_cond   = CAR(args);
  ae_obj_t * const then_branch = CDR(args);
  
  if (log_core) {
    LOG(when_cond,   "when");
    LOG(then_branch, "then");
  }

  bool cond_result = ! NILP(RETURN_IF_ERRORP(EVAL(env, when_cond)));

  if (log_core)
    LOG(TRUTH(cond_result), "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");

    RETURN(ae_core_progn(env, then_branch, LENGTH(then_branch)));
  }

  if (log_core)
    SLOG("chose nil");

  END_DEF_CORE_FUN(when);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _unless
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(unless) {
  ae_obj_t * const unless_cond = CAR(args);
  ae_obj_t * const then_branch = CDR(args);
  
  if (log_core) {
    LOG(unless_cond, "unless");
    LOG(then_branch, "then");
  }

  bool cond_result = NILP(RETURN_IF_ERRORP(EVAL(env, unless_cond)));

  if (log_core)
    LOG(TRUTH(cond_result), "cond_result: ");

  if (cond_result) {
    if (log_core)
      LOG(then_branch, "chose then");

    RETURN(ae_core_progn(env, then_branch, LENGTH(then_branch)));
  }

  if (log_core)
    SLOG("chose nil");

  END_DEF_CORE_FUN(unless);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _or
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(or) {
  
  FOR_EACH(option, args) {
    ret = RETURN_IF_ERRORP(EVAL(env, option));

    if (log_core)
      LOG(ret, "or option");
    
    if (! NILP(ret))
      break;
  }
      
  END_DEF_CORE_FUN(or);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _and
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(and) {
  FOR_EACH(option, args) {
    ret = RETURN_IF_ERRORP(EVAL(env, option));

    if (log_core)
      LOG(ret, "and option");

    if (NILP(ret))
      break;
  }

  END_DEF_CORE_FUN(and);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _while
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(while) {
  ae_obj_t * const while_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  
  if (log_core) {
    LOG(while_cond, "while");
    LOG(do_branch,  "do");
  }
  
  UNTIL_NILP(RETURN_IF_ERRORP(EVAL(env, while_cond))) {
    if (log_core)
      LOG(do_branch, "do while");

    ret = RETURN_IF_ERRORP(ae_core_progn(env, do_branch, LENGTH(do_branch)));
  }

  if (log_core)
    SLOG("left while");
  
  END_DEF_CORE_FUN(while);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _until
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(until) {
  ae_obj_t * const until_cond = CAR(args);
  ae_obj_t * const do_branch  = CDR(args);
  
  if (log_core) {
    LOG(until_cond, "until");
    LOG(do_branch,  "do");
  }
  
  UNTIL_NOT_NILP(RETURN_IF_ERRORP(EVAL(env, until_cond))) {
    if (log_core)
      LOG(do_branch, "do until");

    ret = RETURN_IF_ERRORP(ae_core_progn(env, do_branch, LENGTH(do_branch)));
  }

  if (log_core)
    SLOG("left until");
  
  END_DEF_CORE_FUN(until);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _repeat
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(repeat) {
  ae_obj_t * const first_arg = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
 
  REQUIRE(INTEGERP(first_arg) && (INT_VAL(first_arg) >= 0),
          "repeat requires a positive integer as its first argument");

  const long long int times = INT_VAL(first_arg);

  for (long long int ix = 0; ix < times; ix++)
    ret = RETURN_IF_ERRORP(ae_core_progn(env, CDR(args), LENGTH(CDR(args))));

  END_DEF_CORE_FUN(repeat);
}

