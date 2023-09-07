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

void ae_list_node_init(ae_list_node_t * const ae_list_node) {
  ae_list_node->object = 0;
  ae_list_node->tail   = 0;
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

const char * const ae_list_node_str(const ae_list_node_t * const ae_list_node) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu, %zu)",
    ae_list_node->object, 
    ae_list_node->tail
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

ae_list_node_t * ae_list_node_create(ae_object_t * const ae_object) {
  ae_list_node_t * list_node = malloc(sizeof(ae_list_node_t));
  ae_list_node_init(list_node);
  list_node->object = ae_object;
  return list_node;
}

void ae_list_node_append(ae_list_node_t * const ae_list_node, ae_object_t * const ae_object) {
  ae_list_node_t * position = ae_list_node;
  for (; position->tail; position = position->tail);
  position->tail = ae_list_node_create(ae_object);
}

void ae_list_append(ae_list_t * const ae_list, ae_object_t * const ae_object) {
  if (*ae_list)
    ae_list_node_append(*ae_list, ae_object);
  else 
    *ae_list = ae_list_node_create(ae_object);
}

