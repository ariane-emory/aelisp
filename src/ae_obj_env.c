#include "ae_obj_env.h"

ae_obj_t *ae_env_find(ae_obj_t * const this, ae_obj_t * const symbol) {
  ASSERT_ENVP(this);
  
  ae_obj_t * pos = this;
  
  for (; NOT_NILP(pos); pos = pos->parent) {
    ae_obj_t * symbols = pos->symbols;
    ae_obj_t * values  = pos->values;
        
    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values))
      if (EQ(CAR(symbols), symbol))
        return CAR(values);

    if (EQ(symbols, symbol))
      return values;
  }

  return NIL;
}

ae_obj_t * ae_env_add(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t * value) {
  ae_obj_t * symbols = CONS_NEW(symbol);
  ae_obj_t * values  = CONS_NEW(value);
  
  CDR(symbols)  = this->symbols;
  this->symbols = symbols;
  CDR(values)   = this->values;
  this->values  = values;

  return value;
}
