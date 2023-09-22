#include "ae_obj_env.h"

#define NL (putchar('\n'))
#define PR(...) (fprintf(stdout, __VA_ARGS__))

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
    WRITE(symbol);
    PR(" in env ");
    PUT(this);
    NL;
#endif
    
    ae_obj_t * symbols = ENV_SYMS(pos);
    ae_obj_t * values  = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    PR("  symbols: ");
    WRITE(symbols);
    NL;
    PR("  values:  ");
    WRITE(values);
    NL;
#endif

    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values))
      if (EQ(CAR(symbols), symbol))
        return CAR(values);

    if (EQ(symbols, symbol))
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
  WRITE(symbol);
  PR(" in env ");
  PUT(this);
  PR(" to place %018p.", value);
  NL;
  PR("  syms: ");
  WRITE(ENV_SYMS(this));
  NL;
  PR("  vals:  ");
  WRITE(ENV_VALS(this));
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
        WRITE(sym);
        NL;
        PR("    syms ");
        WRITE(ENV_SYMS(position));
        NL;
        PR("    vals ");
        WRITE(vals);
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

