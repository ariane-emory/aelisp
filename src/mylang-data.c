#include <stdio.h>
#include "mylang-data.h"

void mylang_object_init(mylang_object_t * const mylang_object) {
  mylang_object->type  = ML_INVALID;
  mylang_object->c_str = 0;
}

void mylang_object_print(const mylang_object_t * const mylang_object) {
  printf(
    "('%s', '%s')\n",
    mylang_type_name(mylang_object->type),
    mylang_object->c_str
  );
}

#define return_str(x) case x: return #x;

const char * mylang_type_name(const mylang_type_t mylang_type) {
  switch (mylang_type) {
    FOR_LEXED_TYPES_DO(return_str);
  default: return "UNRECOGNIZED!";
  }
}
