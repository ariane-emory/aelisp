#include <stdbool.h>

#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

// Helper function to check if a form is a quote form
static bool IS_QUOTE_FORM(ae_obj_t * obj) {
  return CONSP(obj) && (CAR(obj) ==  SYM("quote")) && CONSP(CDR(obj));
}

// Helper function to re-quote an argument
static ae_obj_t * REQUOTE(ae_obj_t * obj) {
    return CONS(SYM("quote"), CONS(obj, NIL));
}

ae_obj_t * ae_core_apply(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
    CORE_BEGIN("apply");

    if (log_core)
        LOG(args, "flattening apply's args:");

    ae_obj_t * const new_expr      = CONS(CAR(args), NIL);
    ae_obj_t *       new_expr_tail = new_expr;
    ae_obj_t *       pos           = CDR(args);

    while (!NILP(CDR(pos))) {
        ae_obj_t * arg = CAR(pos);

        // Handle quoted arguments
        if (IS_QUOTE_FORM(arg)) {
            arg = CADR(arg); // Get the actual content inside (quote ...)
        }

        ae_obj_t * const elem = CONS(REQUOTE(arg), NIL);
        CDR(new_expr_tail)    = elem;
        new_expr_tail         = elem;
        pos                   = CDR(pos);
    }

    // Handle the last argument
    ae_obj_t * last = CAR(pos);
    REQUIRE(env, args, PROPERP(last), "apply requires a proper list as its final argument");

    // If the last argument is a quoted list, get its content
    if (IS_QUOTE_FORM(last)) {
        last = CADR(last);
    }

    while (!NILP(last)) {
        ae_obj_t * const elem = CONS(REQUOTE(CAR(last)), NIL);
        CDR(new_expr_tail)    = elem;
        new_expr_tail         = elem;
        last                  = CDR(last);
    }

    if (log_core)
        LOG(new_expr, "flattened apply's args:");

    ae_obj_t * const ret = EVAL(env, new_expr);

    if (log_core)
        OLOG(ret);

    CORE_RETURN("apply", ret);
}
