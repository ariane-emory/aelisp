#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_object.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

void ae_object_init(ae_object_t * const this) {
  memset(this, 0, sizeof(ae_object_t));
  this->type  = AE_INVALID;
  // this->c_str = 0;
}

////////////////////////////////////////////////////////////////////////////////
// _str method
////////////////////////////////////////////////////////////////////////////////

const char * const ae_object_str(const ae_object_t * const this) {
  static char buff[BUFF_LEN] = {0};

  if (this->type == AE_LIST) 
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, %d)",
      this,
      ae_type_str(this->type),
      ae_list_length(&this->data.list_value)
    );
  else
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, [%s])",
      this,
      ae_type_str(this->type),
      this->c_str
    );

  return buff;
}

////////////////////////////////////////////////////////////////////////////////
// _move method
////////////////////////////////////////////////////////////////////////////////

void ae_object_move(ae_object_t * const this, ae_object_t * const that) {
  memcpy(this, that, sizeof(ae_object_t));
  ae_object_init(that);
}

////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////

ae_object_t * ae_object_clone(ae_object_t * const this) {
  ae_object_t * clone = malloc(sizeof(ae_object_t));
  memcpy(clone, this, sizeof(ae_object_t));

  clone->c_str = malloc(strlen(this->c_str) + 1);
  strcpy(clone->c_str, this->c_str);

  switch (this->type) {
  case AE_STRING:
  case AE_SYMBOL:
    clone->data.string_value = malloc(strlen(this->data.string_value) + 1);
    strcpy(clone->data.string_value, this->data.string_value);
  case AE_LIST:
    if (!this)
      return clone;
    
    for (ae_list_node_t * position = this->data.list_value;
         position;
         position = position->tail) {
      ae_object_t * obj_in_list = position->object;
      ae_object_t * clone_of_obj_in_list = ae_object_clone(position->object);
      
      /* void * tmp = fun(position->object); */
      /* assert(tmp != position->object); */
      /* ae_list_push_back(this, tmp); */
    }
  }
  
  return clone;
}
