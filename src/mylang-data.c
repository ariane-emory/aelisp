#include <stdio.h>
#include "mylang-data.h"

void mylang_object_init(mylang_object_t * const mylang_object) {
  mylang_object->type  = ML_INVALID;
  mylang_object->c_str = 0;
}

void mylang_object_print(const mylang_object_t * const mylang_object) {
  printf("(%d, \"%s\")\n", mylang_object->type, mylang_object->c_str);
}
