#include <stdio.h>
#include <stdlib.h>

#include "ae_object.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

void ae_object_init(ae_object_t * const this) {
  this->type  = ML_INVALID;
  this->c_str = 0;
}

////////////////////////////////////////////////////////////////////////////////
// _str methods
////////////////////////////////////////////////////////////////////////////////

const char * const ae_object_str(const ae_object_t * const this) {
  static char buff[BUFF_LEN] = {0};

  if (this->type == ML_LIST) 
    snprintf(
      buff,
      BUFF_LEN,
      "(%s, %d)",
      ae_type_str(this->type),
      ae_list_length(&this->data.list_value)
    );
  else
    snprintf(
      buff,
      BUFF_LEN,
      "(%s, [%s])",
      ae_type_str(this->type),
      this->c_str
    );

  return buff;
}
