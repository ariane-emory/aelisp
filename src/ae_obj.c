#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"

#define BUFF_LEN 256
#define NL      fputc('\n', stream)
#define BSPC    fputc('\b', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)

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

void ae_obj_init(ae_obj_t * const this, ae_type_t type) {
  memset(this, 0, sizeof(ae_obj_t));
  this->type  = type;
}

////////////////////////////////////////////////////////////////////////////////
// _unsafe_move method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
  memcpy(this, that, sizeof(ae_obj_t));
  ae_obj_init(that, AE_INVALID);
}

////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(const ae_obj_t * const this) {
  ae_obj_t * clone = malloc(sizeof(ae_obj_t));
  memcpy(clone, this, sizeof(ae_obj_t));

  switch (this->type) {
  case AE_STRING:
    clone->str_value = malloc(strlen(this->str_value) + 1);
    strcpy(clone->str_value, this->str_value);
    break;
  case AE_SYMBOL:
    clone->sym_value = malloc(strlen(this->sym_value) + 1);
    strcpy(clone->sym_value, this->sym_value);
    break;
  case AE_LIST:
    clone->list_value = 0;
    if (!this)
      return clone;
    for (ae_node_t * position = this->list_value;
         position;
         position = position->tail) {
      ae_obj_t * clone_of_obj_in_list = ae_obj_clone(position->object);
      ae_list_push_back(&clone->list_value, clone_of_obj_in_list);
    }
    break;
  default:
    (void)0; // do nothing special for this type.
  }
  
  return clone;
}

////////////////////////////////////////////////////////////////////////////////
// object's fputs / puts
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "<%p>(%s, [", this, ae_type_str(this->type));
  switch (this->type) {
  case AE_LIST:
    if (this->list_value)
      fprintf(stream, "%d, %p, %p",
              ae_list_node_length(this->list_value),
              this->list_value->object,
              this->list_value->tail);
    else
      fputs("0, nil", stream);
    break;
  default:
    ae_obj_fwrite(this, stream);
    BSPC;
  }
  fprintf(stream, "])");
}

void ae_obj_put(const ae_obj_t * const this) {
  ae_obj_fput(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// object's fput_bytes / put_bytes
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput_bytes(const ae_obj_t * const this, FILE * stream) {
#define same_size_type long int
  size_t max = sizeof(ae_obj_t) / sizeof(same_size_type *);
  
  const same_size_type * start = (same_size_type *)this;

  for (size_t ix = 0; ix < max; ix++)  {
    switch (ix) {
    case 0:
      fprintf(stream, "type ");
      break;
    case 1:
      fprintf(stream, "data ");
      break;
    }
    fprintf(stream, "%016x ", start[ix]);
  }
    
  (void)start; (void)stream;
}

void ae_obj_put_bytes(const ae_obj_t * const this) {
  ae_obj_fput_bytes(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// object's _write methods
////////////////////////////////////////////////////////////////////////////////

void ae_obj_write(const ae_obj_t * const this) {
  ae_obj_fwrite(this, stdout);
}

void ae_obj_fwrite(const ae_obj_t * const this, FILE * stream) {
  switch (this->type) {
  case AE_LIST:
    if (this->type == AE_LIST && this->list_value && this->list_value->object) {
      LPAR;
      ae_obj_list_each((ae_list_t *)&this->list_value, (ae_obj_list_each_fun)ae_obj_write);
      BSPC;
      RPAR;
    }
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL:
    fputs(this->sym_value, stream);
    break;
  case AE_STRING:
    fputc('"', stream);
    fputs(this->str_value, stream);
    fputc('"', stream);
    break;
  case AE_INTEGER:
    fprintf(stream, "%d", this->int_value);
    break;
  case AE_RATIONAL:
    fprintf(stream, "%d/%d", this->numerator_value, this->denominator_value);
    break;
  case AE_FLOAT:
    fprintf(stream, "%g", this->float_value);
    break;
  case AE_CHAR:
  {
    char tmp[3] = { 0 };

    switch (this->char_value) {
#define escaped_char_case(displayed, unescaped)                                                                                             \
      case unescaped:                                                                                                                      \
        tmp[0] = '\\';                                                                                                                      \
        tmp[1] = displayed;                                                                                                                 \
        break;
      FOR_ESCAPED_CHARACTER_DO(escaped_char_case);
#undef escaped_char_case
    default:
      tmp[0] = this->char_value;
    }

    fputc('\'', stream);
    fputs(tmp, stream);
    fputc('\'', stream);
    
    break;
  }
  default:
    fprintf(stream, "UNPRINTABLE");
  }
  SPC;
}
