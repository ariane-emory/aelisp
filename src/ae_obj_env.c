#include "ae_obj_env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_add(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t * value) {
  ae_obj_t * symbols = CONS_NEW(symbol);
  ae_obj_t * values  = CONS_NEW(value);
  
  CDR(symbols)  = this->symbols;
  this->symbols = symbols;
  CDR(values)   = this->values;
  this->values  = values;

  return value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t *ae_env_set(ae_obj_t * this, ae_obj_t * symbol, ae_obj_t  * values) {
  ae_obj_t * pos = this;
  
  while (true) {
    ae_obj_t * symbols = this->symbols;
    ae_obj_t * vals    = this->values;

    for (; CONSP(symbols); symbols = CDR(symbols), vals = CDR(vals)) {
      if (EQ(CAR(symbols), symbol))
        return CAR(vals) = values;
      if (EQ(CDR(symbols),symbol))
        return CDR(vals) = values;
    }

    if (NILP(pos->parent))
      return ae_env_add(pos, symbol, values);
    else
      pos = pos->parent;
  }
}

