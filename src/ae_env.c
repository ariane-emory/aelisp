#include "ae_env.h"
#include "ae_eval.h"
#include "ae_core.h"
#include "ae_util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _add
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_add(ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));
  assert((! NULLP(value)));
  
  ENV_SYMS(env) = CONS(symbol, ENV_SYMS(env));
  ENV_VALS(env) = CONS(value,  ENV_VALS(env));

#ifdef AE_LOG_ENV
  LOG(symbol,    "[adding]");
#endif

  INDENT;

#ifdef AE_LOG_ENV
  LOG(value,   "to value");
  LOG(env,     "in env");
#endif

  OUTDENT;

#ifdef AE_LOG_ENV
  LOG(symbol,    "[done adding]");
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_find(ae_obj_t * const env, ae_obj_t * const symbol) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));

#ifdef AE_LOG_ENV
  LOG(symbol, "[looking up]");
#endif
  
  INDENT;

  ae_obj_t * ret = NIL;
  
  if (NILP(symbol)) {

#ifdef AE_LOG_ENV
    PR("\n\nfound NIL automatically.");
#endif

    ret = NIL;

    goto end;
  }

  if (TRUEP(symbol)) {

#ifdef AE_LOG_ENV
    PR("\n\nfound TRUE automatically.");
#endif
    
    ret = TRUE;

    goto end;
  }
  
  ae_obj_t * pos = env;
  
  for (; ENVP(pos); pos = ENV_PARENT(pos)) {

#ifdef AE_LOG_ENV
    LOG(env, "in env");
#endif
    
    ae_obj_t * symbols = ENV_SYMS(pos);
    ae_obj_t * values  = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    LOG(symbols, "with syms");
#endif

    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values))
      if (EQ(symbol, CAR(symbols))) {

#ifdef AE_LOG_ENV
        LOG(CAR(values), "found it ->"); 
#endif
        
        ret = CAR(values);

        goto end;
      }

    if (EQ(symbol, symbols)) {
      ret = values;

      goto end;
    }
  }
  
#ifdef AE_LOG_ENV
  SLOG("didn't find it!");
#endif
  
end:
  
  OUTDENT;

#ifdef AE_LOG_ENV
  LOG(ret, "[rtrn looked up]");
#endif
  
  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_set(ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));
  assert((! NULLP(value)));

#ifdef AE_LOG_ENV
  LOG(symbol,    "[setting]");
#endif

  INDENT;

  /* ae_obj_t * symbols = ENV_SYMS(env); */
  /* ae_obj_t * values  = ENV_VALS(env); */
  ae_obj_t * pos     = env;

#ifdef AE_LOG_ENV
  LOG(value,   "to value");
#endif

  while (! NILP(pos)) {
    {
      ae_obj_t * syms = ENV_SYMS(pos);
      ae_obj_t * vals = ENV_VALS(pos);

#ifdef AE_LOG_ENV
      LOG(pos,  "in env");
      LOG(syms, "with syms");
#endif

      while (!NILP(syms) && !NILP(vals)) {
        ae_obj_t * sym = CAR(syms);


        if (EQ(symbol, sym)) {

#ifdef AE_LOG_ENV
          LOG(syms, "found it in ->");
#endif

          CAR(vals) = value;

#ifdef AE_LOG_ENV
          LOG(vals, "values after");
#endif

          goto end;
        }

        syms = CDR(syms);
        vals = CDR(vals);
      }
    }

    if (NILP(pos->parent)) {

#ifdef AE_LOG_ENV
      NL;
      SLOG("adding new");
#endif

#ifdef AE_LEXICAL_SCOPING      
      ENV_ADD(env, symbol, value);
#else
      ENV_ADD(pos, symbol, value);
#endif

      OUTDENT;
          
      return;
    } else {

#ifdef AE_LOG_ENV
      SLOG("going up");
#endif
      
      OUTDENT;
          
      pos = ENV_PARENT(pos);
    }
  }

end:
  OUTDENT;

  SLOG("[done setting]");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _new_root
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_new_root(void) {
  ae_obj_t * env = NEW_ENV(NIL, NIL, NIL);

#define add_core_fun(name, ...)          ae_env_set(env, SYM(#name), NEW_CORE(#name, &ae_core_##name, false));  
#define add_core_special_fun(name, ...)  ae_env_set(env, SYM(#name), NEW_CORE(#name, &ae_core_##name, true));
#define add_core_op(name, sym, ...)      ae_env_set(env, SYM(#sym),  NEW_CORE(#name, &ae_core_##name, false));
  
  FOR_EACH_CORE_FUN(add_core_fun);
  FOR_EACH_CORE_CMP_OP(add_core_op);
  FOR_EACH_CORE_MATH_OP(add_core_op);
  FOR_EACH_CORE_SPECIAL_FUN(add_core_special_fun);

  // self-evaluating symbols:
  ae_env_set(env, NIL,  NIL);
  ae_env_set(env, TRUE, TRUE);

  // ae_env_define_list_and_quote(env);
  
  return env;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _define_list_fun
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_define_list_and_quote(ae_obj_t * const env) {
  static ae_obj_t * list_def = NULL;
  static ae_obj_t * list_fun = NULL;

  list_def = list_def ?: CONS(SYM("setq"), CONS(SYM("list"), CONS(CONS(SYM("lambda"), CONS(SYM("args"),  CONS(SYM("args"), NIL)  )), NIL)));
  list_fun = list_fun ?: EVAL(env, list_def);

  static ae_obj_t * quote_def = NULL;
  static ae_obj_t * quote_fun = NULL;

  quote_def = quote_def ?: CONS(SYM("setq"), CONS(SYM("quote"), CONS(CONS(SYM("macro"), CONS(CONS(SYM("args"), NIL),  CONS(SYM("args"), NIL)  )), NIL)));
  quote_fun = quote_fun ?: EVAL(env, quote_def);
}
