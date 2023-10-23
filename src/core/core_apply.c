#include <stdbool.h>

#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

static bool IS_QUOTE_FORM(ae_obj_t * obj) {
  return CONSP(obj) && (CAR(obj) ==  SYM("quote")) && CONSP(CDR(obj));
}

static ae_obj_t * REQUOTE(ae_obj_t * obj) {
  return CONS(SYM("quote"), CONS(obj, NIL));
}

ae_obj_t * ae_core_apply(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("apply");

  if (log_core)
    LOG(args, "flattening apply's args:");

  ae_obj_t *       ret           = NIL;
  ae_obj_t * const new_expr      = CONS(CAR(args), NIL);
  ae_obj_t *       new_expr_tail = new_expr;
  ae_obj_t *       pos           = CDR(args);
  ae_obj_t *       evaluated_arg = NIL;
  
  while (!NILP(CDR(pos))) {
    ae_obj_t * arg = CAR(pos);

    if (IS_QUOTE_FORM(arg))
      arg = CADR(arg);

    evaluated_arg         = RETURN_IF_ERRORP(EVAL(env, CAR(pos)));
    ae_obj_t * const elem = CONS(REQUOTE(evaluated_arg), NIL);
    CDR(new_expr_tail)    = elem;
    new_expr_tail         = elem;
    pos                   = CDR(pos);
  }

  ae_obj_t * last = CAR(pos);

  if (IS_QUOTE_FORM(last))
    last = CADR(last);
  else
    last = RETURN_IF_ERRORP(EVAL(env, last));

  REQUIRE(env, args, PROPERP(last), "apply requires a proper list as its final argument");
  
  while (!NILP(last)) {
    ae_obj_t * const elem = CONS(REQUOTE(CAR(last)), NIL);
    CDR(new_expr_tail)    = elem;
    new_expr_tail         = elem;
    last                  = CDR(last);
  }

  if (log_core)
    LOG(new_expr, "flattened apply's args:");

  ret = RETURN_IF_ERRORP(EVAL(env, new_expr));

  if (log_core)
    OLOG(ret);

end:
  
  CORE_RETURN("apply", ret);
}





ae_obj_t * ae_core_apply_alt(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
    CORE_BEGIN("apply");

    if (log_core)
        LOG(args, "flattening apply's args:");

    ae_obj_t * const new_expr      = CONS(CAR(args), NIL);
    ae_obj_t *       new_expr_tail = new_expr;
    ae_obj_t *       pos           = CDR(args);
    ae_obj_t *       evaluated_arg;

    // Iterate through all arguments except the last one
    for (; !NILP(CDR(CDR(pos))); pos = CDR(pos)) { // Ensure CDR(CDR(pos)) is not NIL to skip the last arg
        evaluated_arg = EVAL(env, CAR(pos));
        
        ae_obj_t * const elem = CONS(evaluated_arg, NIL);
        CDR(new_expr_tail)    = elem;
        new_expr_tail         = elem;
    }

    ae_obj_t * last = CAR(pos);

    REQUIRE(env, args, PROPERP(last), "apply requires a proper list as its final argument");

    if (CONSP(last))
        CDR(new_expr_tail) = last;  // Link the new list's tail to the last argument directly.
    else
        CDR(new_expr_tail) = CONS(last, NIL);

    if (log_core)
        LOG(new_expr, "flattened apply's args:");

    ae_obj_t * const ret = EVAL(env, new_expr);

    if (log_core)
        OLOG(ret);

    CORE_RETURN("apply", ret);
}
