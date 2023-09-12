#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ae_list.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init methods
////////////////////////////////////////////////////////////////////////////////

void ae_node_init(ae_node_t * const this) {
  this->head = 0;
  this->tail   = 0;
}

////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////

ae_node_t * ae_node_create(struct ae_obj_t * const obj) {
  ae_node_t * node = malloc(sizeof(ae_node_t));
  ae_node_init(node);
  node->head = obj;
  return node;
}

ae_node_t * ae_node_push_back(ae_node_t * const this, struct ae_obj_t * const obj) {
  ae_node_t * position = this;
  for (; position->tail; position = position->tail);
  position->tail = ae_node_create(obj);
  // printf("After push, length is %d.\n", ae_node_length(this));
  return position->tail;
}

size_t ae_node_length(const ae_node_t * const this) {
  size_t length = 0;
  for (const ae_node_t * position = this; position; position = position->tail, length++);
  return length;
}

void ae_node_each (ae_node_t * const this, ae_node_each_fun fun) {
  for (const ae_node_t * position = this; position; position = position->tail)
    fun(position->head);
}

////////////////////////////////////////////////////////////////////////////////

ae_node_t * ae_list_push_back(ae_list_t * const this, struct ae_obj_t * const obj) {
  return *this
    ? ae_node_push_back(*this, obj)
    : (*this = ae_node_create(obj));
}

