#include "ae_obj_env.h"

#define NL (putchar('\n'))
#define PR(...) (fprintf(stdout, __VA_ARGS__))

////////////////////////////////////////////////////////////////////////////////////////////////////
// _add
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_add(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t * value) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  ASSERT_NOT_NULLP(value);
  
  /* ae_obj_t * symbols = CONS_NIL(symbol); */
  /* ae_obj_t * values  = CONS_NIL(value); */
  
  /* CDR(symbols)       = ENV_SYMS(this); */
  /* ENV_SYMS(this)     = symbols; */
  /* CDR(values)        = ENV_VALS(this); */
  /* ENV_VALS(this)     = values; */

  ENV_SYMS(this)     = CONS(symbol, ENV_SYMS(this));
  ENV_VALS(this)     = CONS(value,  ENV_VALS(this));
  
  return value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_find(ae_obj_t * const this, ae_obj_t * const symbol) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  
  ae_obj_t * pos = this;
  
  for (; NOT_NILP(pos); pos = ENV_PARENT(pos)) {
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

ae_obj_t * ae_env_set(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t * value) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  ASSERT_NOT_NULLP(value);

  
  while (true) {
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

    ae_obj_t * syms      = ENV_SYMS(this);
    ae_obj_t * vals      = ENV_VALS(this);
    ae_obj_t * env       = this;
    
    for (; CONSP(syms); syms = CDR(syms), vals = CDR(vals)) {
#ifdef AE_LOG_ENV
      PR("  Looking at ");
      WRITE(CAR(syms));
      NL;
#endif

      if (EQ(CAR(syms), symbol)) {
#ifdef AE_LOG_ENV
        PR("  Found it in car of: ");
        PUT(vals);
        NL;
        CAR(vals) = value; 
        PR("  After:              ");
        PUT(vals);
        NL;
#endif
        return CAR(vals);
      }
      if (EQ(CDR(syms), symbol)) {
#ifdef AE_LOG_ENV
        PR("  Found it in cdr of: ");
        PUT(vals);
        NL;
        CDR(vals) = value;
        PR("  After:              ");
        PUT(vals);
        NL;
#endif
        return CDR(vals);
      }
    }

    if (NILP(env->parent)) {
#ifdef AE_LOG_ENV
      PR("  Adding new.\n");
#endif
      return ENV_ADD(this, symbol, vals);
    }
    else {
#ifdef AE_LOG_ENV
      PR("  Going up.\n");
#endif
      env = ENV_PARENT(env);
    }
  }
}

