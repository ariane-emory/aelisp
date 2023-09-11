#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"

#define BUFF_LEN 256

////////////////////////////////////////////////////////////////////////////////
// ae_type_str method
////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_LEXED_TYPES_DO(return_str);
    return_str(AE_FREE);
    return_str(AE_INVALID);
  default: return "UNRECOGNIZED!";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_init(ae_obj_t * const this) {
  memset(this, 0, sizeof(ae_obj_t));
  this->type  = AE_INVALID;
}

////////////////////////////////////////////////////////////////////////////////
// _str method
////////////////////////////////////////////////////////////////////////////////

const char * ae_obj_str(const ae_obj_t * const this) {
  static char buff[BUFF_LEN] = {0};

  if (this->type == AE_LIST) 
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, %d)",
      this,
      ae_type_str(this->type),
      ae_list_length(&this->list_value)
    );
  else
    snprintf(
      buff,
      BUFF_LEN,
      "<O %p>(%s, %p [%s])",
      this,
      ae_type_str(this->type),
      this->c_str,
      this->c_str
    );

  return buff;
}


////////////////////////////////////////////////////////////////////////////////
// fputs / puts
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fputs(const ae_obj_t * const this, FILE * stream) {
  const char * tmp = ae_obj_str(this);
  fputs(tmp, stream);
}

void ae_obj_puts(const ae_obj_t * const this) {
  ae_obj_fputs(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// fputsc / putsc
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fputsc(const ae_obj_t * const this, FILE * stream) {
  fputs(this->c_str, stream);
}

void ae_obj_putsc(const ae_obj_t * const this) {
  ae_obj_fputsc(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// _move method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_move(ae_obj_t * const this, ae_obj_t * const that) {
  memcpy(this, that, sizeof(ae_obj_t));
  ae_obj_init(that);
}

////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(ae_obj_t * const this) {
  ae_obj_t * clone = malloc(sizeof(ae_obj_t));
  memcpy(clone, this, sizeof(ae_obj_t));

  if (clone->c_str) {
    clone->c_str = malloc(strlen(this->c_str) + 1);
    strcpy(clone->c_str, this->c_str);
  }
  
  switch (this->type) {
  case AE_STRING:
  case AE_SYMBOL:
    clone->str_value = malloc(strlen(this->str_value) + 1);
    strcpy(clone->str_value, this->str_value);
  case AE_LIST:
    ae_list_init(&clone->list_value);
    
    if (!this)
      return clone;
    
    for (ae_list_node_t * position = this->list_value;
         position;
         position = position->tail) {
      ae_obj_t * clone_of_obj_in_list = ae_obj_clone(position->object);
      ae_list_push_back(&clone->list_value, clone_of_obj_in_list);
    }
  default:
    (void)0; // do nothing special for this type.
  }
  
  return clone;
}
