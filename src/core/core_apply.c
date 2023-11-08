#include <stdbool.h>

#include "core_includes.h"
#include "jump_return.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

static bool is_quote_form(ae_obj_t * obj) {
  return CONSP(obj) && (CAR(obj) ==  SYM("quote")) && CONSP(CDR(obj));
}

static ae_obj_t * requote(ae_obj_t * obj) {
  return CONS(SYM("quote"), CONS(obj, NIL));
}

DEF_CORE_FUN(apply) {
  if (log_core)
    LOG(args, "flattening apply's args:");

  ae_obj_t * const new_expr      = CONS(CAR(args), NIL);
  ae_obj_t *       new_expr_tail = new_expr;
  ae_obj_t *       pos           = CDR(args);
  ae_obj_t *       evaluated_arg = NIL;
  
  UNTIL_NILP(CDR(pos)) {
    ae_obj_t * arg = CAR(pos);

    if (is_quote_form(arg))
      arg = CADR(arg);

    evaluated_arg         = RETURN_IF_ERRORP(EVAL(env, CAR(pos)));
    ae_obj_t * const elem = CONS(requote(evaluated_arg), NIL);
    CDR(new_expr_tail)    = elem;
    new_expr_tail         = elem;
    pos                   = CDR(pos);
  }

  ae_obj_t * last = CAR(pos);

  if (is_quote_form(last))
    last = CADR(last);
  else
    last = RETURN_IF_ERRORP(EVAL(env, last));

  REQUIRE(PROPERP(last),
          "apply requires a proper list as its final argument");
  
  UNTIL_NILP(last) {
    ae_obj_t * const elem = CONS(requote(CAR(last)), NIL);
    CDR(new_expr_tail)    = elem;
    new_expr_tail         = elem;
    last                  = CDR(last);
  }

  if (log_core)
    LOG(new_expr, "flattened apply's args:");

  if (log_core)
    OLOG(ret);

  RETURN(EVAL(env, new_expr));

  END_DEF_CORE_FUN(apply);
}
