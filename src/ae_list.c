#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ae_list.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init methods
////////////////////////////////////////////////////////////////////////////////

void ae_list_init(ae_list_t * const ae_list) {
  *ae_list = 0;
}

void ae_list_node_init(ae_node_t * const this) {
  this->object = 0;
  this->tail   = 0;
}

////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////

ae_node_t * ae_list_node_create(void * const object) {
  ae_node_t * list_node = malloc(sizeof(ae_node_t));
  ae_list_node_init(list_node);
  list_node->object = object;
  return list_node;
}

ae_node_t * ae_list_node_push_back(ae_node_t * const this, void * const object) {
  ae_node_t * position = this;
  for (; position->tail; position = position->tail);
  position->tail = ae_list_node_create(object);
  // printf("After push, length is %d.\n", ae_list_node_length(this));
  return position->tail;
}

size_t ae_list_node_length(const ae_node_t * const this) {
  size_t length = 0;
  for (const ae_node_t * position = this; position; position = position->tail, length++);
  return length;
}

void ae_list_node_each (ae_node_t * const this, ae_obj_list_each_fun fun) {
  for (const ae_node_t * position = this; position; position = position->tail)
    fun(position->object);
}

////////////////////////////////////////////////////////////////////////////////

ae_node_t * ae_list_push_back(ae_list_t * const this, void * const object) {
  return *this
    ? ae_list_node_push_back(*this, object)
    : (*this = ae_list_node_create(object));
}

size_t ae_list_length(const ae_list_t * const this) {
  return *this
    ? ae_list_node_length(*this)
    : 0;
}

void ae_obj_list_each(ae_list_t * const this, ae_obj_list_each_fun fun) {
  if (*this)
    ae_list_node_each(*this, fun);
}
