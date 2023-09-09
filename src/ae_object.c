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

void ae_object_fputs(const ae_object_t * const this, FILE * stream) {
  const char * tmp = ae_object_str(this);
  fputs(tmp, stream);
}

void ae_object_puts(const ae_object_t * const this) {
  ae_object_fputs(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// fputsc / putsc
////////////////////////////////////////////////////////////////////////////////

void ae_object_fputsc(const ae_object_t * const this, FILE * stream) {
  switch(this->type) {
  /* case AE_CHAR: */
  /*   fputc(this->data.char_value, stream); */
  /*   return; */
  /* case AE_FLOAT: */
  /*   fprintf(stream, "%.02lf", this->data.float_value); */
  /*   return; */
  /* case AE_INTEGER: */
  /*   fprintf(stream, "%d", this->data.int_value); */
  /*   return; */
  /* case AE_RATIONAL: */
  /*   fputs(this->c_str, stream); */
  /*   return; */
  /* case AE_STRING: */
  /*   fputc('"', stream); */
  /*   fputs(this->data.string_value, stream); */
  /*   fputc('"', stream); */
  /*   return; */
  /* case AE_SYMBOL: */
  /*   fputs(this->data.string_value, stream); */
  /*   return; */
  default:
    fputs(this->c_str, stream);
    // fputs("<unprintable ae_object>", stream);
  }
}

void ae_object_putsc(const ae_object_t * const this) {
  ae_object_fputsc(this, stdout);
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

// #define REPORT printf("Line %d.\n", __LINE__)
#define REPORT 

ae_object_t * ae_object_clone(ae_object_t * const this) {
  ae_object_t * clone = malloc(sizeof(ae_object_t)); REPORT;
  memcpy(clone, this, sizeof(ae_object_t)); REPORT;

  if (clone->c_str) {
    clone->c_str = malloc(strlen(this->c_str) + 1); REPORT;
    strcpy(clone->c_str, this->c_str); REPORT;
  }
  
  switch (this->type) {
  case AE_STRING:
  case AE_SYMBOL:
    clone->data.string_value = malloc(strlen(this->data.string_value) + 1); REPORT;
    strcpy(clone->data.string_value, this->data.string_value); REPORT;
  case AE_LIST:
    ae_list_init(&clone->data.list_value); REPORT;
    
    if (!this)
      return clone; REPORT;
    
    for (ae_list_node_t * position = this->data.list_value;
         position;
         position = position->tail) {
      ae_object_t * obj_in_list = position->object; REPORT;
      ae_object_t * clone_of_obj_in_list = ae_object_clone(position->object); REPORT;
      ae_list_push_back(&clone->data.list_value, clone_of_obj_in_list); REPORT;
    }
  }
  
  return clone;
}
