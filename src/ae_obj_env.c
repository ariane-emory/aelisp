#include "ae_obj_env.h"

ae_obj_t *ae_env_find(ae_obj_t * const this, ae_obj_t * const symbol) {
  ASSERT_ENVP(this);
  
  ae_obj_t * pos = this;
  
  for (; pos != NIL; pos = pos->parent) {
    ae_obj_t *symbols = pos->symbols;
    ae_obj_t *values = pos->values;
        
    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values))
      if (CAR(symbols) == symbol)
        return CAR(values);

    if (EQ(symbols, symbol))
      return values;
  }

  return NIL;
}

