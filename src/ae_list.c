#include <stdio.h>
#include <stdlib.h>

#include "ae_list.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init methods
////////////////////////////////////////////////////////////////////////////////

void ae_list_init(ae_list_t * const ae_list) {
  *ae_list = 0;
}

void ae_list_node_init(ae_list_node_t * const this) {
  this->object = 0;
  this->tail   = 0;
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

const char * const ae_list_node_str(const ae_list_node_t * const this) {
  static char buff[BUFF_LEN] = {0};

  snprintf(
    buff,
    BUFF_LEN,
    "(%zu, %zu)",
    this->object, 
    this->tail
  );

  return buff;
}

////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////

ae_list_node_t * ae_list_node_create(void * const object) {
  ae_list_node_t * list_node = malloc(sizeof(ae_list_node_t));
  ae_list_node_init(list_node);
  list_node->object = object;
  return list_node;
}

ae_list_node_t * ae_list_node_push_back(ae_list_node_t * const this, void * const object) {
  ae_list_node_t * position = this;
  for (; position->tail; position = position->tail);
  return (position->tail = ae_list_node_create(object));
}

ae_list_node_t * ae_list_push_back(ae_list_t * const this, void * const object) {
  return *this
    ? ae_list_node_push_back(*this, object)
    : (*this = ae_list_node_create(object));
}

void ae_list_node_each (ae_list_node_t * const this, ae_list_node_each_fun fun) {
  for (ae_list_node_t * position = this; position; position = position->tail)
    fun(position->object);
}

void ae_list_each(ae_list_t * const this, ae_list_node_each_fun fun) {
  if (this)
    ae_list_node_each(*this, fun);
}

ae_list_t ae_list_map(ae_list_t * const this, ae_list_node_map_fun fun) {
  ae_list_t new_list = 0;

  if (this)
    for (ae_list_node_t * position = *this; position; position = position->tail)
      ae_list_push_back(&new_list, fun(position->object));

  return new_list;
}
