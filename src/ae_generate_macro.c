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
  ae_obj_t* args_part = INTERN("args");
  // printf("%-16s", "args_part"); PRINC(args_part); NL;

  // (null args)
  ae_obj_t* null_args_expr = CONS(INTERN("null"), CONS(args_part, NIL));
  // printf("%-16s", "null_args_expr"); PRINC(null_args_expr); NL;j

  // (quote if)
  ae_obj_t* quote_if = CONS(INTERN("quote"), CONS(INTERN("if"), NIL));
  // printf("%-16s", "quote_if"); PRINC(quote_if); NL;j

  // (quote and)
  ae_obj_t* quote_and = CONS(INTERN("quote"), CONS(INTERN("and"), NIL));
  // printf("%-16s", "quote_and"); PRINC(quote_and); NL;j

  // (cons (quote and) (cdr args))
  ae_obj_t* cons_quote_and = CONS(INTERN("cons"), CONS(quote_and, CONS(CONS(INTERN("cdr"), CONS(args_part, NIL)), NIL)));
  // printf("%-16s", "cons_quote_and"); PRINC(cons_quote_and); NL;j

  // (list (quote if) (car args) (cons (quote and) (cdr args)))
  ae_obj_t* inner_list_expr = CONS(INTERN("t"), CONS(CONS(INTERN("list"), CONS(quote_if, CONS(CONS(INTERN("car"), CONS(args_part, NIL)), CONS(cons_quote_and, NIL)))), NIL));
  // printf("%-16s", "inner_list_expr"); PRINC(inner_list_expr); NL;j

  // (null args)
  ae_obj_t* null_args = CONS(CONS(INTERN("null"), CONS(args_part, NIL)), CONS(TRUE, NIL));
  // printf("%-16s", "null_args"); PRINC(null_args); NL;j

  // (null (cdr args))
  ae_obj_t* null_cdr_args = CONS(CONS(INTERN("null"), CONS(CONS(INTERN("cdr"), CONS(args_part, NIL)), NIL)), CONS(CONS(INTERN("car"), CONS(args_part, NIL)), NIL));
  // printf("%-16s", "null_cdr_args"); PRINC(null_cdr_args); NL;j

  // (list (quote if) (car args) (cons (quote and) (cdr args)))
  ae_obj_t* cond_expr = CONS(INTERN("cond"), CONS(null_args, CONS(null_cdr_args, CONS(inner_list_expr, NIL))));
  // printf("%-16s", "cond_expr"); PRINC(cond_expr); NL;j

  // (defmacro and args (cond ...))
  ae_obj_t* final_expr = CONS(
    INTERN("defmacro"),
    CONS(INTERN("and"),
         CONS(args_part,
              CONS(cond_expr,
                   NIL))));

  return final_expr;
}

ae_obj_t * ae_generate_macro_or      (void) {
  return NIL;
}
