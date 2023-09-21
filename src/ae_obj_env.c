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

ae_obj_t *ae_env_set(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t  * val) {
  ae_obj_t * pos = this;
  
  while (true) {
    ae_obj_t * symbols = this->symbols;
    ae_obj_t * vals    = this->values;

    for (; CONSP(symbols); symbols = CDR(symbols), vals = CDR(vals)) {
      if (EQ(CAR(symbols), symbol))
        return CAR(vals) = val;
      if (EQ(CDR(symbols),symbol))
        return CDR(vals) = val;
    }

    if (NILP(pos->parent))
      return ae_env_add(pos, symbol, val);
    else
      pos = pos->parent;
  }
}

