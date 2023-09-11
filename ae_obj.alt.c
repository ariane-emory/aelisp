#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "ae_obj.h"

////////////////////////////////////////////////////////////////////////////////
// ae_type's _str method
////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_LEXED_TYPES_DO(return_str);
    return_str(AE_INVALID);
  default: return "UNRECOGNIZED!";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////
// list objs' _push_back method
////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t;
void ae_obj_put         (const struct ae_obj_t * const this);
void ae_obj_put_bytes   (const struct ae_obj_t * const this);

void ae_obj_list_push_back(ae_obj_t * const this, ae_obj_t * const ae_object) {
  /* fputs("Push into ", stdout); */
  /* ae_obj_put(this); */
  /* fflush(stdout); */
  // assert(this->type == AE_LIST);
  
  fputs("\nPushing ", stdout);
  ae_obj_put(ae_object); 
  fputs(" into ", stdout);
  ae_obj_put(this); putchar('\n');

  ae_obj_t * position = this;
  
  if (position->head) {
    for (; position->tail; position = position->tail);
    ae_obj_t * node = malloc(sizeof(ae_obj_t));
    ae_obj_init(node, AE_LIST);
    node->head = ae_object;
    position->tail = node;
  }
  else {
    position->head = ae_object;    
  }
}

////////////////////////////////////////////////////////////////////////////////
// list objs' _length method
////////////////////////////////////////////////////////////////////////////////

size_t ae_obj_list_length(const ae_obj_t * const this) {
  assert(this->type == AE_LIST);
  
  if (! this->head)
    return 0;
  size_t length = 0;
  for (const ae_obj_t * position = this; position; position = position->tail, length++);
  return length;
}

////////////////////////////////////////////////////////////////////////////////
// list objs' _each method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_list_each (ae_obj_t * const this, ae_obj_list_each_fun fun) {
  assert(this->type == AE_LIST);
    
  if (this->head)
    for (const ae_obj_t * position = this; position; position = position->tail)
      fun(position->head);
}
////////////////////////////////////////////////////////////////////////////////
// object's _write method
////////////////////////////////////////////////////////////////////////////////
  
#define NL      fputc('\n', stream)
#define BSPC    fputc('\b', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)

////////////////////////////////////////////////////////////////////////////////
// object's _init method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_init(ae_obj_t * const this, ae_type_t type) {
  memset(this, 0, sizeof(ae_obj_t));
  this->type  = type;
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
    if (this->type == AE_LIST && this->head) {
      LPAR;
      ae_obj_list_each((ae_obj_t *)this, (ae_obj_list_each_fun)ae_obj_write);
      BSPC;
      RPAR;
    }
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL:
    fputs(this->symbol_value, stream);
    break;
  case AE_STRING:
    fputc('"', stream);
    fputs(this->string_value, stream);
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

////////////////////////////////////////////////////////////////////////////////
// object's fputs / puts
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "<%p>(%s, [", this, ae_type_str(this->type));
  switch (this->type) {
  case AE_LIST:
    fprintf(stream, "%d, %p, %p",
            ae_obj_list_length(this),
            this->head,
            this->tail);
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
// object's _move method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
  memcpy(this, that, sizeof(ae_obj_t));
  ae_obj_init(that, AE_INVALID);
}

////////////////////////////////////////////////////////////////////////////////
// object's _clone method
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(const ae_obj_t * const this) {
  ae_obj_t * clone = malloc(sizeof(ae_obj_t));
  memcpy(clone, this, sizeof(ae_obj_t));

  fputs("Cloning      ", stdout);
  ae_obj_put(this);
  putchar('\n');
  
  switch (this->type) {
  case AE_STRING:
    clone->string_value = malloc(strlen(this->string_value) + 1);
    strcpy(clone->string_value, this->string_value);
  case AE_SYMBOL:
    clone->symbol_value = malloc(strlen(this->symbol_value) + 1);
    strcpy(clone->symbol_value, this->symbol_value);
  case AE_LIST:
    clone->head = 0;
    clone->tail = 0;
    
    if (!this->head) {
      puts("Stop.");
      return clone;
    }
    else {
      puts("Continue.");
    }
    
    for (const ae_obj_t * position = this;
         position->type == AE_LIST && position->tail;
         position = position->tail) {
      puts("Iter.");
      ae_obj_t * clone_of_obj_in_list = ae_obj_clone(position->head);
      // if (clone->list_value.head)
        ae_obj_list_push_back(clone, clone_of_obj_in_list);
    }
  default:
    (void)0; // do nothing special for this type.
  }
  
  return clone;
}
