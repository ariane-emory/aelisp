#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_object.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

void ae_object_init(ae_object_t * const this) {
  memset(this, 0, sizeof(ae_object_t));
  this->type  = AE_INVALID;
  // this->c_str = 0;
}

////////////////////////////////////////////////////////////////////////////////
// _str method
////////////////////////////////////////////////////////////////////////////////

const char * const ae_object_str(const ae_object_t * const this) {
  static char buff[BUFF_LEN] = {0};

  if (this->type == AE_LIST) 
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, %d)",
      this,
      ae_type_str(this->type),
      ae_list_length(&this->data.list_value)
    );
  else
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, [%s])",
      this,
      ae_type_str(this->type),
      this->c_str
    );

  return buff;
}

////////////////////////////////////////////////////////////////////////////////
// _move method
////////////////////////////////////////////////////////////////////////////////

void ae_object_move(ae_object_t * const this, ae_object_t * const that) {
  memcpy(this, that, sizeof(ae_object_t));
  ae_object_init(that);
}
