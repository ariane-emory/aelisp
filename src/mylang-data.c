#include <stdio.h>
#include <stdlib.h>
#include "mylang-data.h"

void mylang_object_init(mylang_object_t * const mylang_object) {
  mylang_object->type  = ML_INVALID;
  mylang_object->c_str = 0;
}

#define return_str(x) case x: return #x;

const char * const mylang_type_str(const mylang_type_t mylang_type) {
  switch (mylang_type) {
    FOR_LEXED_TYPES_DO(return_str);
  default: return "UNRECOGNIZED!";
  }
}

const char * const mylang_object_str(const mylang_object_t * const mylang_object) {
#define BUFF_LEN 256

  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%s, [%s])",
    mylang_type_str(mylang_object->type),
    mylang_object->c_str
  );

  return buff;
}

void mylang_list_init(mylang_list_t * const mylang_list) {
  mylang_list->head = 0;
  mylang_list->tail = 0;
}

void mylang_list_append(mylang_list_t * const mylang_list, mylang_object_t * mylang_object) {
  mylang_list_t * position = mylang_list;

  for (; position->tail; position = position->tail);

  mylang_list_t * new_tail = malloc(sizeof(mylang_list_t));
  mylang_list_init(new_tail);
  position->tail = new_tail;
}
