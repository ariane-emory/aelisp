#include <stdio.h>
#include <stdlib.h>
#include "ae-data.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init methods
////////////////////////////////////////////////////////////////////////////////

void ae_object_init(ae_object_t * const ae_object) {
  ae_object->type  = ML_INVALID;
  ae_object->c_str = 0;
}

void ae_list_init(ae_list_t * const ae_list) {
  *ae_list = 0;
}

void ae_list_item_init(ae_list_item_t * const ae_list_item) {
  ae_list_item->object = 0;
  ae_list_item->tail   = 0;
}

////////////////////////////////////////////////////////////////////////////////
// _str methods
////////////////////////////////////////////////////////////////////////////////

const char * const ae_list_str(const ae_list_t * const ae_list) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu)",
    (*ae_list)
  );

  return buff;
}

const char * const ae_list_item_str(const ae_list_item_t * const ae_list_item) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu, %zu)",
    ae_list_item->object, 
    ae_list_item->tail
  );

  return buff;
}

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

////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////

void ae_list_item_append(ae_list_item_t * const ae_list_item, ae_object_t * ae_object) {
  ae_list_item_t * position = ae_list_item;

  for (; position->tail; position = position->tail);

  ae_list_item_t * new_tail = malloc(sizeof(ae_list_item_t));
  ae_list_item_init(new_tail);
  new_tail->object = ae_object;
  position->tail = new_tail;
}

void ae_list_append(ae_list_t * const ae_list, ae_object_t * ae_object) {

  if (*ae_list) {
    ae_list_item_append(*ae_list, ae_object);
  }
  else {
    ae_list_item_t * new_tail = malloc(sizeof(ae_list_item_t));
    ae_list_item_init(new_tail);
    new_tail->object = ae_object;
    *ae_list = new_tail;
  }
}

