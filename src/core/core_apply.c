#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_apply(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("apply");

  if (log_core)
    LOG(args, "flattening apply's args:");
  
  ae_obj_t * const new_expr      = CONS(CAR(args), NIL);
  ae_obj_t *       new_expr_tail = new_expr;
  ae_obj_t *       pos           = CDR(args);

  for (; !NILP(CDR(pos)); pos = CDR(pos)) {
    if (log_core) {
      OLOG(pos);
      OLOG(new_expr);
    }
        
    ae_obj_t * const elem = CONS(CAR(pos), NIL);
    CDR(new_expr_tail)    = elem;
    new_expr_tail         = elem;    
  } 

  ae_obj_t * last = CAR(pos);

  if (log_core) {
    LOG(pos, "final pos");
    LOG(new_expr, "prefinal new_expr");
  }

  REQUIRE(env, args, PROPERP(last), "apply requires a proper list as its final argument");
  
  if (CONSP(last))
    CDR(new_expr_tail) = last;  // Link the new list's tail to the last argument directly.
  else
    CDR(new_expr_tail) = CONS(last, NIL);

  if (log_core)
    LOG(new_expr, "final new_expr");

  ae_obj_t * const ret = EVAL(env, new_expr);

  if (log_core)
    OLOG(ret);
  
  CORE_RETURN("apply", ret);
}


