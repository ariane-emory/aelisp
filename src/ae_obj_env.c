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

ae_obj_t * ae_env_set(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t * values) {
  ASSERT_ENVP(this);
  ASSERT_SYMBOLP(symbol);
  ASSERT_NOT_NULLP(values);

  ae_obj_t *   pos       = this;
  
  while (true) {
    ae_obj_t * symbols   = ENV_SYMS(this);
    ae_obj_t * vals      = ENV_VALS(this);

    for (; CONSP(symbols); symbols = CDR(symbols), vals = CDR(vals)) {
      if (EQ(CAR(symbols), symbol))
        return CAR(vals) = values;
      if (EQ(CDR(symbols),symbol))
        return CDR(vals) = values;
    }

    if (NILP(pos->parent))
      return ENV_ADD(pos, symbol, values);
    else
      pos = ENV_PARENT(pos);
  }
}

