#include "ae_generate_macro.h"
#include "ae_list.h"

ae_obj_t * ae_generate_macro_defmacro(void) {
  // (quote setq):
  ae_obj_t* quote_setq = CONS(SYM("quote"), CONS(SYM("setq"), NIL));
    
  //  (name params . body):
  ae_obj_t* args_part = CONS(SYM("name"), NEW_CONS(SYM("params"), SYM("body"))); 

  // (quote macro):
  ae_obj_t* quote_macro = CONS(SYM("quote"), CONS(SYM("macro"), NIL));

  // ((list (quote setq) name (list (quote macro) params . body))):
  ae_obj_t* list_expr = CONS(CONS(SYM("list"), CONS(quote_setq, CONS(SYM("name"), CONS(CONS(SYM("list"), CONS(quote_macro, NEW_CONS(SYM("params"), SYM("body")))), NIL)))), NIL);

  // (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body)))):
  ae_obj_t* final_expr = CONS(SYM("setq"), CONS(SYM("defmacro"), CONS(CONS(SYM("macro"), CONS(args_part, list_expr)), NIL)));

  return final_expr;
}

ae_obj_t * ae_generate_macro_defun(void) {
  // (name params . body):
  ae_obj_t* args_part = CONS(SYM("name"), NEW_CONS(SYM("params"), SYM("body")));

  // (quote setq):
  ae_obj_t* quote_setq = CONS(SYM("quote"), CONS(SYM("setq"), NIL));

  // (quote lambda):
  ae_obj_t* quote_lambda = CONS(SYM("quote"), CONS(SYM("lambda"), NIL));

  // (list (quote lambda) params . body):
  ae_obj_t* inner_list = CONS(SYM("list"), CONS(quote_lambda, NEW_CONS(SYM("params"), SYM("body"))));

  // (list (quote setq) name (list (quote lambda) params . body)):
  ae_obj_t* list_expr = CONS(SYM("list"), CONS(quote_setq, CONS(SYM("name"), CONS(inner_list, NIL))));

  // (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body))):
  ae_obj_t* final_expr = CONS(SYM("defmacro"), CONS(SYM("defun"), CONS(args_part, CONS(list_expr, NIL))));

  return final_expr;
}

ae_obj_t * ae_generate_macro_and(void) {
  // args
  ae_obj_t* args_part = SYM("args");

  // (null args)
  ae_obj_t* null_args_expr = CONS(SYM("null"), CONS(args_part, NIL));
  // printf("%-16s", "null_args_expr"); PRINC(null_args_expr); NL;j

  // (quote if)
  ae_obj_t* quote_if = CONS(SYM("quote"), CONS(SYM("if"), NIL));
  // printf("%-16s", "quote_if"); PRINC(quote_if); NL;j

  // (quote and)
  ae_obj_t* quote_and = CONS(SYM("quote"), CONS(SYM("and"), NIL));
  // printf("%-16s", "quote_and"); PRINC(quote_and); NL;j

  // (cons (quote and) (cdr args))
  ae_obj_t* cons_quote_and = CONS(SYM("cons"), CONS(quote_and, CONS(CONS(SYM("cdr"), CONS(args_part, NIL)), NIL)));
  // printf("%-16s", "cons_quote_and"); PRINC(cons_quote_and); NL;j

  // (list (quote if) (car args) (cons (quote and) (cdr args)))
  ae_obj_t* inner_list_expr = CONS(SYM("t"), CONS(CONS(SYM("list"), CONS(quote_if, CONS(CONS(SYM("car"), CONS(args_part, NIL)), CONS(cons_quote_and, NIL)))), NIL));
  // printf("%-16s", "inner_list_expr"); PRINC(inner_list_expr); NL;j

  // (null args)
  ae_obj_t* null_args = CONS(CONS(SYM("null"), CONS(args_part, NIL)), CONS(TRUE, NIL));
  // printf("%-16s", "null_args"); PRINC(null_args); NL;j

  // (null (cdr args))
  ae_obj_t* null_cdr_args = CONS(CONS(SYM("null"), CONS(CONS(SYM("cdr"), CONS(args_part, NIL)), NIL)), CONS(CONS(SYM("car"), CONS(args_part, NIL)), NIL));
  // printf("%-16s", "null_cdr_args"); PRINC(null_cdr_args); NL;j

  // (list (quote if) (car args) (cons (quote and) (cdr args)))
  ae_obj_t* cond_expr = CONS(SYM("cond"), CONS(null_args, CONS(null_cdr_args, CONS(inner_list_expr, NIL))));
  // printf("%-16s", "cond_expr"); PRINC(cond_expr); NL;j

  // (defmacro and args (cond ...))
  ae_obj_t* final_expr = CONS(
    SYM("defmacro"),
    CONS(SYM("and"),
         CONS(args_part,
              CONS(cond_expr,
                   NIL))));

  return final_expr;
}

ae_obj_t * ae_generate_macro_or(void) {
  ae_obj_t* args_part = SYM("args");

  // (null args)
  ae_obj_t* null_args = CONS(SYM("null"), CONS(args_part, NIL));

  // (quote nil)
  ae_obj_t* quote_nil = CONS(SYM("quote"), CONS(SYM("nil"), NIL));

  // (quote cond)
  ae_obj_t* quote_cond = CONS(SYM("quote"), CONS(SYM("cond"), NIL));

  // (mapcar list args)
  ae_obj_t* mapcar_expr = CONS(SYM("mapcar"), CONS(SYM("list"), CONS(args_part, NIL)));

  // (cons (quote cond) (mapcar list args))
  ae_obj_t* cons_expr = CONS(SYM("cons"), CONS(quote_cond, CONS(mapcar_expr, NIL)));

  // (if (null args) nil (cons (quote cond) (mapcar list args)))
  ae_obj_t* if_expr = CONS(SYM("if"), CONS(null_args, CONS(NIL, CONS(cons_expr, NIL))));

  // (defmacro or args ...)
  ae_obj_t* final_expr = CONS(SYM("defmacro"), CONS(SYM("or"), CONS(args_part, CONS(if_expr, NIL))));

  return final_expr;
}
