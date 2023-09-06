#include <stdio.h>
#include "mylang-data.h"

void mylang_object_init(mylang_object_t * const mylang_object) {
  mylang_object->type  = ML_INVALID;
  mylang_object->c_str = 0;
}

void mylang_object_print(const mylang_object_t * const mylang_object) {
  printf(
    "(%d, %s, \"%s\")\n",
    mylang_object->type,
    mylang_type_name(mylang_object->type),
    mylang_object->c_str
  );
}

const char * mylang_type_name(const mylang_type_t mylang_type) {
  switch (mylang_type) {
  default:
    return "Unrecognized type";
  }
}
