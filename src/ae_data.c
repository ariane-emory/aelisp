#include <stdio.h>
#include <stdlib.h>

#include "ae_data.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

void ae_object_init(ae_object_t * const ae_object) {
  ae_object->type  = ML_INVALID;
  ae_object->c_str = 0;
}

////////////////////////////////////////////////////////////////////////////////
// _str methods
////////////////////////////////////////////////////////////////////////////////

const char * const ae_object_str(const ae_object_t * const ae_object) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%s, [%s])",
    ae_type_str(ae_object->type),
    ae_object->c_str
  );

  return buff;
}

#define return_str(x) case x: return #x;
const char * const ae_type_str(const ae_type_t ae_type) {
  switch (ae_type) {
    FOR_LEXED_TYPES_DO(return_str);
  default: return "UNRECOGNIZED!";
  }
}
#undef return_str
