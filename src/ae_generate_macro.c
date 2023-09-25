#include "ae_generate_macro.h"
#include "ae_list.h"

#undef DOT
#define DOT NEW_CONS

ae_obj_t * ae_generate_macro_defmacro(void) {
  // (quote setq):
  ae_obj_t* quote_setq = CONS(INTERN("quote"), CONS(INTERN("setq"), NIL));
    
  //  (name params . body):
  ae_obj_t* args_part = CONS(INTERN("name"), DOT(INTERN("params"), INTERN("body"))); 

  // (quote macro):
  ae_obj_t* quote_macro = CONS(INTERN("quote"), CONS(INTERN("macro"), NIL));

  // ((list (quote setq) name (list (quote macro) params . body))):
  ae_obj_t* list_expr = CONS(CONS(INTERN("list"), CONS(quote_setq, CONS(INTERN("name"), CONS(CONS(INTERN("list"), CONS(quote_macro, DOT(INTERN("params"), INTERN("body")))), NIL)))), NIL);

  // (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body)))):
  ae_obj_t* final_expr = CONS(INTERN("setq"), CONS(INTERN("defmacro"), CONS(CONS(INTERN("macro"), CONS(args_part, list_expr)), NIL)));

  return final_expr;
}

ae_obj_t * ae_generate_macro_defun   (void) {
  // (name params . body):
  ae_obj_t* args_part = CONS(INTERN("name"), DOT(INTERN("params"), INTERN("body")));

  // (quote setq):
  ae_obj_t* quote_setq = CONS(INTERN("quote"), CONS(INTERN("setq"), NIL));

  // (quote lambda):
  ae_obj_t* quote_lambda = CONS(INTERN("quote"), CONS(INTERN("lambda"), NIL));

  // (list (quote lambda) params . body):
  ae_obj_t* inner_list = CONS(INTERN("list"), CONS(quote_lambda, DOT(INTERN("params"), INTERN("body"))));

  // (list (quote setq) name (list (quote lambda) params . body)):
  ae_obj_t* list_expr = CONS(INTERN("list"), CONS(quote_setq, CONS(INTERN("name"), CONS(inner_list, NIL))));

  // (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body))):
  ae_obj_t* final_expr = CONS(INTERN("defmacro"), CONS(INTERN("defun"), CONS(args_part, CONS(list_expr, NIL))));

  return final_expr;
}

ae_obj_t * ae_generate_macro_and     (void) {
  return NIL;
}

ae_obj_t * ae_generate_macro_or      (void) {
  return NIL;
}
