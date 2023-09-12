#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj.h"

#define NL      fputc('\n', stream)
#define BSPC    fputc('\b', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)
#define DQUO    fputc('"',  stream)
#define SQUO    fputc('\'', stream)

////////////////////////////////////////////////////////////////////////////////
// ae_type_str method
////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_LEXED_TYPES_DO(return_str);
    return_str(AE_FREE);
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
  fputs("Inited  ", stdout);
  ae_obj_put(this);
  putchar('\n');
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
  fputs("Clone   ", stdout);
  ae_obj_put(this);
  putchar('\n');
  fflush(stdout);
  
  ae_obj_t * clone = ALLOC_AE_OBJ;
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
    clone->head = 0;
    clone->tail = 0;
    if (!this->head)
      return clone;
    for (ae_obj_t * position = (ae_obj_t *)this;
         position;
         position = position->tail) {
      ae_obj_t * clone_of_obj_in_list = ae_obj_clone(position->head);
      ae_obj_push_back(clone, clone_of_obj_in_list);
    }
    break;
  default:
    (void)0; // do nothing special for this type.
  }
  
  return clone;
}

////////////////////////////////////////////////////////////////////////////////
// obj's fputs / puts
////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "<%p>(%s, ", this, ae_type_str(this->type));
  
  switch (this->type) {
  case AE_INVALID:
  case AE_FREE:
    BSPC; BSPC; RPAR; return;
  case AE_LIST:
    if (this->head)
      fprintf(stream, "length %d, %p, %p",
              ae_obj_length(this),
              this->head,
              this->tail);
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL:
    ae_obj_fwrite(this, stream);
    BSPC;
    break;
  case AE_STRING:
    ae_obj_fwrite(this, stream);
    BSPC;
    break;
  case AE_CHAR:
  case AE_FLOAT:
  case AE_INTEGER:
  case AE_RATIONAL:
    ae_obj_fwrite(this, stream);
    BSPC;
    break;
  default:
    LSQR;
    ae_obj_fwrite(this, stream);
    BSPC;
    RSQR;
  }
  RPAR;
}

void ae_obj_put(const ae_obj_t * const this) {
  ae_obj_fput(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////
// obj's fput_bytes / put_bytes
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
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////

void ae_obj_write(const ae_obj_t * const this) {
  ae_obj_fwrite(this, stdout);
}

void ae_obj_fwrite(const ae_obj_t * const this, FILE * stream) {
  switch (this->type) {
  case AE_LIST:
    if (this->type == AE_LIST && this->head) {
      LPAR;
      ae_obj_each((ae_obj_t *)this, (ae_obj_each_fun)ae_obj_write);
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
    if (this->str_value == 0) {
      fputs("(null)", stream);
    }
    else {
      DQUO;
      fputs(this->str_value, stream);
      DQUO;
    }
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

    SQUO;
    fputs(tmp, stream);
    SQUO;
    
    break;
  }
  default:
    fprintf(stream, "UNPRINTABLE");
  }
  
  SPC;
}

////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_create(ae_obj_t * const obj) {
  ae_obj_t * node = ALLOC_AE_OBJ;
  ae_obj_init(node, AE_LIST);
  node->head = obj;
  return node;
}

size_t ae_obj_length(const ae_obj_t * const this) {
  size_t length = 0;
  for (const ae_obj_t * position = this; position; position = position->tail, length++);
  return length;
}

void ae_obj_each (ae_obj_t * const this, ae_obj_each_fun fun) {
  for (const ae_obj_t * position = this; position; position = position->tail)
    fun(position->head);
}

////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////

void ae_obj_push_back(ae_obj_t * const this, ae_obj_t * const obj) {
  if (this->head) {
    ae_obj_t * position = this;
    for (; position->tail; position = position->tail);
    position->tail = ae_obj_create(obj);
  }
  else {
    this->head = obj;
  }
}

////////////////////////////////////////////////////////////////////////////////
// pool 
////////////////////////////////////////////////////////////////////////////////

ae_obj_t pool[POOL_SIZE];

ae_obj_t * pool_alloc_ae_obj() {
  for (size_t ix = 0; ix < POOL_SIZE; ix++) {
    ae_obj_t * obj = &pool[ix];

    if (obj->type != AE_FREE)
      continue;
      
    ae_obj_init(obj, AE_INVALID);

#define BUFF_LEN 5
    char buff[BUFF_LEN] = { 0 };
    snprintf(buff, BUFF_LEN, "#%d:", ix); // off by one? I forget.
#undef BUFF_LEN
      
    return obj;
  }
    
  printf("ERROR: Pool is full.\n");
  return 0;
}

void pool_free_ae_obj(ae_obj_t * const this) {
  ae_obj_init(this, AE_FREE);
}
