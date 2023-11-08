#include "core_includes.h"

#include "env.h"
#include "eval.h"
#include "jump_return.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _let
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(let) {
  ae_obj_t * const varlist = CAR(args);

  REQUIRE(env, args, PROPERP(varlist),    "varlist must be a proper list");
  // REQUIRE(env, args, LENGTH(varlist) > 0, "empty varlist");

  FOR_EACH(varlist_item, varlist) {
    REQUIRE(env, args,
            SYMBOLP(varlist_item) || (CONSP(varlist_item) && LENGTH(varlist_item) == 2),
            "varlist items must be symbols or lists with two elements");

    ae_obj_t * const sym = SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item);

    REQUIRE(env, args, (! SPECIAL_SYMP(sym)), "let forms cannot bind special symbols");

  }
  
  ae_obj_t * const body    = CDR(args);

  REQUIRE(env, args, PROPERP(body),       "body must be a proper list");
  // REQUIRE(env, args, LENGTH(body) > 0,    "empty body");
  
  ae_obj_t * const new_env = NEW_ENV(env, NIL, NIL);

  int ctr = 0;
  
  FOR_EACH(varlist_item, varlist) {
    ctr++;

    if (log_core) 
      LOG(varlist_item, "let varlist item #%d/%d", ctr, LENGTH(varlist));
    
    INDENT;
    
    if (log_core)
      OLOG(varlist_item);

    ae_obj_t * val = SYMBOLP(varlist_item)
      ? NIL
      : RETURN_IF_ERRORP(EVAL(env, CADR(varlist_item)));

    // let only sets the last-bound-to property if it's not already set.
    if ((LAMBDAP(val) || MACROP(val)) && ! HAS_PROP("last-bound-to", CAR(varlist_item)))
      PUT_PROP(CAR(varlist_item), "last-bound-to", val);

    if (log_core) {
      if (SYMBOLP(varlist_item))
        LOG(varlist_item, "binding symbol");
      else 
        LOG(CAR(varlist_item), "binding symbol");
   
      LOG(val,               "to value");   
    }

    ENV_SET_L(new_env, SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item), val);

    OUTDENT;
  }

  if (log_core) {
    LOG(ENV_SYMS(new_env), "new_env syms");
    LOG(ENV_VALS(new_env), "new_env vals");
  }

  RETURN(ae_core_progn(new_env, body, LENGTH(body)));

  END_DEF_CORE_FUN(let);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _let_star
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(let_star) {
  ae_obj_t * const varlist = CAR(args);

  REQUIRE(env, args, PROPERP(varlist),    "varlist must be a proper list");
  // REQUIRE(env, args, LENGTH(varlist) > 0, "empty varlist");

  FOR_EACH(varlist_item, varlist) {
    REQUIRE(env, args,
            SYMBOLP(varlist_item) || (CONSP(varlist_item) && LENGTH(varlist_item) == 2),
            "varlist items must be symbols or lists with two elements");

    ae_obj_t * const sym = SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item);

    REQUIRE(env, args, (! SPECIAL_SYMP(sym)), "let forms cannot bind special symbols");

  }

  ae_obj_t * const body    = CDR(args);

  REQUIRE(env, args, PROPERP(body),       "body must be a proper list");
  // REQUIRE(env, args, LENGTH(body) > 0,    "empty body");
  
  ae_obj_t * const new_env = NEW_ENV(env, NIL, NIL);

  int ctr = 0;
  
  FOR_EACH(varlist_item, varlist) {
    ctr++;

    if (log_core)
      LOG(varlist_item, "let* varlist item #%d/%d", ctr, LENGTH(varlist));

    INDENT;
    
    if (log_core)
      OLOG(varlist_item);

    ae_obj_t * val = SYMBOLP(varlist_item)
      ? NIL
      : RETURN_IF_ERRORP(EVAL(new_env, CADR(varlist_item)));

    // let only sets the last-bound-to property if it's not already set.
    if ((LAMBDAP(val) || MACROP(val)) && ! HAS_PROP("last-bound-to", CAR(varlist_item)))
      PUT_PROP(CAR(varlist_item), "last-bound-to", val);


    if (log_core) {
      if (SYMBOLP(varlist_item))
        LOG(varlist_item, "binding symbol");
      else 
        LOG(CAR(varlist_item), "binding symbol");
   
      LOG(val,               "to value");   
    }
    
    ENV_SET_L(new_env, SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item), val);

    OUTDENT;
  }

  if (log_core) {
    LOG(ENV_SYMS(new_env), "new_env syms");
    LOG(ENV_VALS(new_env), "new_env vals");
  }

  RETURN(ae_core_progn(new_env, body, LENGTH(body)));

  END_DEF_CORE_FUN(let_star);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _letrec
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(letrec) {
  static ae_obj_t * dummy   = NULL;

  if (!dummy)
    dummy = KW("DUMMY");
  
  ae_obj_t * const  varlist = CAR(args);

  REQUIRE(env, args, PROPERP(varlist),    "varlist must be a proper list");
  // REQUIRE(env, args, LENGTH(varlist) > 0, "empty varlist");
 
  FOR_EACH(varlist_item, varlist) {
    REQUIRE(env, args,
            SYMBOLP(varlist_item) || (CONSP(varlist_item) && LENGTH(varlist_item) == 2),
            "varlist items must be symbols or lists with two elements");

    ae_obj_t * const sym = SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item);

    REQUIRE(env, args, (! SPECIAL_SYMP(sym)), "let forms cannot bind special symbols");

  }
  
  ae_obj_t * const body    = CDR(args);

  REQUIRE(env, args, PROPERP(body),       "body must be a proper list");
  // REQUIRE(env, args, LENGTH(body) > 0,    "empty body");

  ae_obj_t * const new_env = NEW_ENV(env, NIL, NIL);

  // First, bind all symbols to dummy values in the environment.
  FOR_EACH(varlist_item, varlist) {
    ENV_SET_L(new_env, CAR(varlist_item), dummy);  // DUMMY_VALUE can be anything like NIL or a specific uninitialized marker.
  }

  // Now, go through the list again and evaluate the right-hand side expressions to replace the dummy values.
  FOR_EACH(varlist_item, varlist) {
    if (log_core)
      LOG(varlist_item, "letrec varlist item");

    INDENT;

    ae_obj_t * val = SYMBOLP(varlist_item)
      ? NIL
      : RETURN_IF_ERRORP(EVAL(new_env, CADR(varlist_item)));

    // let only sets the last-bound-to property if it's not already set.
    if ((LAMBDAP(val) || MACROP(val)) && ! HAS_PROP("last-bound-to", CAR(varlist_item)))
      PUT_PROP(CAR(varlist_item), "last-bound-to", val);

    if (log_core) {
      if (SYMBOLP(varlist_item))
        LOG(varlist_item, "binding symbol");
      else 
        LOG(CAR(varlist_item), "binding symbol");
   
      LOG(val, "to value");   
    }
    
    ENV_SET_L(new_env, SYMBOLP(varlist_item) ? varlist_item : CAR(varlist_item), val);

    OUTDENT;
  }

  if (log_core) {
    LOG(ENV_SYMS(new_env), "new_env syms");
    LOG(ENV_VALS(new_env), "new_env vals");
  }

  RETURN(ae_core_progn(new_env, body, LENGTH(body)));

  END_DEF_CORE_FUN(letrec);
}
