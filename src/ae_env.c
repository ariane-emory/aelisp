#include "ae_env.h"
#include "ae_eval.h"
#include "ae_core.h"
#include "ae_util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _add
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_add(ae_obj_t * const this, ae_obj_t * const symbol, ae_obj_t * const value) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  ASSERT_NOT_NULLP(value);

#ifdef AE_LOG_ENV
  PR("\nAdding %018p", value);
  PUT(value);
  PR(" to %018p", this);
  PUT(this);
  NL;
#endif
  
  ENV_SYMS(this) = CONS(symbol, ENV_SYMS(this));
  ENV_VALS(this) = CONS(value,  ENV_VALS(this));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_find(ae_obj_t * const this, ae_obj_t * const symbol) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  
  ae_obj_t * pos = this;
  
  for (; ENVP(pos); pos = ENV_PARENT(pos)) {
#ifdef AE_LOG_ENV
    PR("Looking for '");
    PRINC(symbol);
    PR(" in env ");
    PUT(this);
    NL;
#endif
    
    ae_obj_t * symbols = ENV_SYMS(pos);
    ae_obj_t * values  = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    PR("  symbols: ");
    PRINC(symbols);
    NL;
    PR("  values:  ");
    PRINC(values);
    NL;
#endif

    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values))
      if (EQ(symbol, CAR(symbols))) {
#ifdef AE_LOG_ENV
        LOG(CAR(values), "Found it =>"); 
#endif
        
        return CAR(values);
      }

    if (EQ(symbol, symbols))
      return values;
  }

  return NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_set(ae_obj_t * const this, ae_obj_t * const symbol, ae_obj_t * const value) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  ASSERT_NOT_NULLP(value);

#ifdef AE_LOG_ENV
  PR("Looking for '");
  PRINC(symbol);
  PR(" in env ");
  PUT(this);
  PR(" to place %018p.", value);
  NL;
  PR("  syms: ");
  PRINC(ENV_SYMS(this));
  NL;
  PR("  vals:  ");
  PRINC(ENV_VALS(this));
  NL;
  NL;
#endif

  ae_obj_t * env    = this;

  while (true) {
    {
      ae_obj_t * vals = ENV_VALS(env);

      FOR_EACH(sym, ENV_SYMS(env)) {
#ifdef AE_LOG_ENV
        PR("  Looking at sym ");
        PRINC(sym);
        NL;
        PR("    syms ");
        PRINC(ENV_SYMS(position));
        NL;
        PR("    vals ");
        PRINC(vals);
        NL;
#endif
        
        if (EQ(symbol, sym)) {
#ifdef AE_LOG_ENV
          PR("  Found it in car of: ");
          PUT(vals);
          NL;
#endif
          
          CAR(vals) = value;

#ifdef AE_LOG_ENV
          PR("  After:              ");
          PUT(vals);
          NL;
#endif
          
          return;
        }
        
        if (EQ(symbol, CDR(ENV_SYMS(position)))) {
#ifdef AE_LOG_ENV
          PR("  Found it in cdr of: ");
          PUT(vals);
          NL;
#endif
          
          CDR(vals) = value;
          
#ifdef AE_LOG_ENV
          PR("  After:              ");
          PUT(vals);
          NL;
#endif
          
          return;
        }
        
        vals = CDR(vals);
      }
    }
    
    if (NILP(env->parent)) {
#ifdef AE_LOG_ENV
      PR("  Adding new.\n");
#endif
      
        ENV_ADD(this, symbol, value);
        
        return;
    }
    else {
#ifdef AE_LOG_ENV
      PR("  Going up.\n");
#endif
      
      env = ENV_PARENT(env);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _new_root
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_new_root(void) {
  ae_obj_t * env = NEW_ENV(NIL, NIL, NIL);

#define add_core_fun(name, ...)          ae_env_set(env, INTERN(#name), NEW_CORE(#name, &ae_core_##name, false));  
#define add_core_special_fun(name, ...)  ae_env_set(env, INTERN(#name), NEW_CORE(#name, &ae_core_##name, true));
#define add_core_op(name, sym, ...)      ae_env_set(env, INTERN(#sym),  NEW_CORE(#name, &ae_core_##name, false));
  
  FOR_EACH_CORE(add_core_fun);
  FOR_EACH_CMP_OP(add_core_op);
  FOR_EACH_MATH_OP(add_core_op);
  FOR_EACH_CORE_SPECIAL_FUN(add_core_special_fun);
  ae_env_set(env, NIL,  NIL);
  ae_env_set(env, TRUE, TRUE);

  return env;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _define_list_fun
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_define_list_fun(ae_obj_t * const env) {
  static ae_obj_t * list_def = NULL;
  static ae_obj_t * list_fun = NULL;

  list_def = list_def ?: CONS(INTERN("setq"), CONS(INTERN("list"), CONS(CONS(INTERN("lambda"), CONS(INTERN("args"),  CONS(INTERN("args"), NIL)  )), NIL)));
  list_fun = list_fun ?: EVAL(env, list_def);

  return list_fun;
}

