#include <stdio.h>
#include <stdlib.h>
#include "mylang-data.h"

#define BUFF_LEN 256

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

const char * const mylang_list_str(const mylang_list_t * const mylang_list) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu)",
    (*mylang_list)
  );

  return buff;
}

const char * const mylang_list_item_str(const mylang_list_item_t * const mylang_list_item) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu, %zu)",
    mylang_list_item->object, 
    mylang_list_item->tail
           );

  return buff;
}

void mylang_list_init(mylang_list_t * const mylang_list) {
  (*mylang_list)  = 0;
}

void mylang_list_item_init(mylang_list_item_t * const mylang_list_item) {
  mylang_list_item->object = 0;
  mylang_list_item->tail   = 0;
}

void mylang_list_item_append(mylang_list_item_t * const mylang_list_item, mylang_object_t * mylang_object) {
  mylang_list_item_t * position = mylang_list_item;

  for (; position->tail; position = position->tail);

  mylang_list_item_t * new_tail = malloc(sizeof(mylang_list_item_t));
  mylang_list_item_init(new_tail);
  new_tail->object = mylang_object;
  position->tail = new_tail;
}

void mylang_list_append(mylang_list_t * const mylang_list, mylang_object_t * mylang_object) {

  if (*mylang_list) {
    mylang_list_item_append(*mylang_list, mylang_object);
  }
  else {
    mylang_list_item_t * new_tail = malloc(sizeof(mylang_list_item_t));
    mylang_list_item_init(new_tail);
    new_tail->object = mylang_object;
    (*mylang_list) = new_tail;
  }
}

