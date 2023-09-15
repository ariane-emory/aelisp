#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// write helpers
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NL      fputc('\n', stream)
#define BSPC    fputc('\b', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)
#define DQUO    fputc('"',  stream)
#define SQUO    fputc('\'', stream)

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_type_str method
////////////////////////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_EACH_LEXED_TYPE(return_str);
    return_str(AE_FREE____);
  default:
    return "UNRECOGNIZED";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_init(ae_obj_t * const this, ae_type_t type) {
#ifdef NOISY_INIT
  fputs("Initializing     ", stdout);
  ae_obj_put(this);
  putchar('\n');
#endif

  memset(this, 0, sizeof(ae_obj_t));
  this->type  = type;

#ifdef NOISY_INIT
  fputs("Initialized      ", stdout);
  ae_obj_put(this);
  putchar('\n');
#endif

  return this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _unsafe_move method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
  assert(this != that);
  
#ifdef NOISY_INIT
  fputs("Moving           ", stdout);
  ae_obj_put(that);
  fputs(" to ", stdout);
  ae_obj_put(this);
  putchar('\n');
#endif

  memcpy(this, that, sizeof(ae_obj_t));
  ae_obj_init(that, AE_FREE____); // AE_INVALID_);

#ifdef NOISY_INIT
  fputs("Moved            ", stdout);
  ae_obj_put(that);
  fputs(" to ", stdout);
  ae_obj_put(this);
  putchar('\n');
#endif

  return this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(const ae_obj_t * const this) {
#ifdef NOISY_INIT
  fputs("Cloning          ", stdout);
  ae_obj_put(this);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * clone = NULL;

#define CLONE_USING_MEMCPY clone = ALLOC(); memcpy(clone, this, sizeof(ae_obj_t))
#define DUP_C_STR(field) clone->field = strdup(this->field)
  
  switch (this->type) {
  case AE_CONS____:
    clone = ae_list_map((ae_obj_t *)this, (ae_list_map_fun)ae_obj_clone);
    break;
  case AE_STRING__:
    CLONE_USING_MEMCPY;
    DUP_C_STR(str_value);
    break;
  case AE_SYMBOL__:
    CLONE_USING_MEMCPY;
    DUP_C_STR(sym_value);
    break;
  default:
    CLONE_USING_MEMCPY;
  }

#undef CLONE_USING_MEMCPY
#undef DUP_C_STR
  
#ifdef NOISY_INIT
  fputs("Cloned           ", stdout);
  ae_obj_put(this);
  fputs(" into ", stdout);
  ae_obj_put(clone);
  putchar('\n');
  fflush(stdout);
#endif

  return clone;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "%011p[ %s ", this, ae_type_str(this->type));
  // fprintf(stream, "<%011p>(%s, ", this, ae_type_str(this->type));
  
  switch (this->type) {
  case AE_LPAREN__:
  case AE_RPAREN__:
  case AE_INVALID_:
  case AE_FREE____:
  case AE_INF_____:
    BSPC; break;
  case AE_CONS____:
    if (! CAR(this))
      fputs("nil", stream);
    else if (! CDR(this))
      fprintf(stream, "%011p %-11p %2d",
              CAR(this) ,
              CDR(this),
              ae_list_length(this));
    else
      fprintf(stream, "%011p %-011p %2d",
              CAR(this) ,
              CDR(this),
              ae_list_length(this));    
    break;
  case AE_SYMBOL__:
  case AE_STRING__:
  case AE_CHAR____:
  case AE_FLOAT___:
  case AE_INTEGER_:
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
  SPC;
  RSQR;
}

void ae_obj_put(const ae_obj_t * const this) {
  ae_obj_fput(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_bytes / put_bytes
////////////////////////////////////////////////////////////////////////////////////////////////////

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
}

void ae_obj_put_bytes(const ae_obj_t * const this) {
  ae_obj_fput_bytes(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_write(const ae_obj_t * const this) {
  ae_obj_fwrite(this, stdout);
}

static FILE * _stream;

static void ae_obj_fwrite_internal(const ae_obj_t * const this) {
  switch (this->type) {
  case AE_INF_____:
    fputs("∞", _stream);
    break;
  case AE_CONS____:
    if (this->type == AE_CONS____ && CAR(this) ) {
      fputc('(', _stream);
      ae_list_each((ae_obj_t *)this, (ae_list_each_fun)ae_obj_fwrite_internal);
      fputc('\b', _stream);
      fputc(')', _stream);
    }
    else
      fputs("nil", _stream);
    break;
  case AE_SYMBOL__:
    fputs(this->sym_value, _stream);
    break;
  case AE_STRING__:
    if (this->str_value == NULL) {
      fputs("(null)", _stream);
    }
    else {
      fputc('"', _stream);
      fputs(this->str_value, _stream);
      fputc('"', _stream);
    }
    break;
  case AE_INTEGER_:
    fprintf(_stream, "%d", this->int_value);
    break;
  case AE_RATIONAL:
    fprintf(_stream, "%d/%d", this->numerator_value, this->denominator_value);
    break;
  case AE_FLOAT___:
    fprintf(_stream, "%g", this->float_value);
    break;
  case AE_CHAR____:
  {
    char tmp[3] = { 0 };

    switch (this->char_value) {
#define escaped_char_case(displayed, unescaped)                                                                                             \
      case unescaped:                                                                                                                       \
        tmp[0] = '\\';                                                                                                                      \
        tmp[1] = displayed;                                                                                                                 \
        break;
      FOR_EACH_ESCAPED_CHARACTER(escaped_char_case);
#undef escaped_char_case
    default:
      tmp[0] = this->char_value;
    }

    fputc('\'', _stream);
    fputs(tmp, _stream);
    fputc('\'', _stream);
    
    break;
  }
  default:
    fprintf(_stream, "UNPRINTABLE");
  }
  
  fputc(' ', _stream);
}

void ae_obj_fwrite(const ae_obj_t * const this, FILE * stream) {
  _stream = stream;
  ae_obj_fwrite_internal(this);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////////////////////////

size_t ae_list_length(const ae_obj_t * const list) {
  ASSERT_CONSP(list);
  
  size_t length = 0;

  if (! CAR(list) ) return 0;
  
  for (const ae_obj_t * position = list; position; position = CDR(position), length++);

  return length;
}

// #define   AE_OBJ_EACH_RECURSES

ae_obj_t * ae_list_map(ae_obj_t * const list, ae_list_map_fun fun) {
  if (! list)
    return NULL;
  
  ASSERT_CONSP(list);

  return CONS(fun(CAR(list) ), ae_list_map(CDR(list), fun));
}

void ae_list_each (ae_obj_t * const list, ae_list_each_fun fun) {
#ifdef    AE_OBJ_EACH_RECURSES
  if (! list) return;

  ASSERT_CONSP(list);

  fun(CAR(list) );

  ae_list_each(CDR(list), fun);
#else  // AE_OBJ_EACH_RECURSES
  ASSERT_CONSP(list);

  for (const ae_obj_t * position = list; position; position = CDR(position))
    fun(CAR(position) );
#endif // AE_OBJ_EACH_RECURSES
}

bool ae_list_has_member(const ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);

   for (const ae_obj_t * position = list; position; position = CDR(position))
    if (EQ(CAR(position), member))
      return true;
  
   return false;
}

ae_obj_t * ae_list_remove_member(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);

  ae_obj_t * new_list = NULL;
  
  for (const ae_obj_t * position = list; position; position = CDR(position)) {
    if (EQ(CAR(position), member))
      continue;
    else if (! new_list)
      new_list = CONS(CAR(position), NULL);
    else 
      PUSH(new_list, CAR(position));
  }
  
  return new_list;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  assert(head != tail); // consing an obj onto itself is not yet supported.
  
  if (tail)
    ASSERT_CONSP(tail);

#ifdef NOISY_INIT
  printf("Cons %p %p\n", head, tail);
  fflush(stdout);
#endif
  
  ae_obj_t * new_list = NEW(AE_CONS____);

  CAR(new_list) = head;
  CDR(new_list) = tail;

  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef NOISY_INIT
#  define AFTER_PUSH_MESSAGE(tailtip)                                                                                                       \
    fputs("Pushed           ", stdout);                                                                                                     \
    ae_obj_put(obj);                                                                                                                        \
    fputs(" into ", stdout);                                                                                                                \
    ae_obj_put(this);                                                                                                                       \
    fputs("' tailtip ", stdout);                                                                                                            \
    ae_obj_put(tailtip);                                                                                                                    \
    putchar('\n');                                                                                                                          \
    putchar('\n');
#else
#  define AFTER_PUSH_MESSAGE(tailtip) ((void)NULL)
#endif

ae_obj_t * ae_list_push_back(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);
 
#ifdef NOISY_INIT
  fputs("Pushing          ", stdout);
  ae_obj_put(member);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
#endif
  
  if (CAR(list)) {
    ae_obj_t * position = list;
    for (; CDR(position); position = CDR(position));
    CDR(position)       = CONS(member, NULL);

    AFTER_PUSH_MESSAGE(CDR(position));
  
    return CDR(position);
  }
  else {
    CAR(list) = member;

    AFTER_PUSH_MESSAGE(list);

    return list;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NEW_SYM(sym) ae_obj_t * sym = NEW(AE_SYMBOL__); sym->sym_value = strdup(string)

ae_obj_t * ae_list_intern_string(ae_obj_t ** const sym_list_p, ae_string_t string) {
  if (! *sym_list_p)
    *sym_list_p = NEW(AE_CONS____);
  
  if (! CAR(*sym_list_p)) {
    // shortcut/hack for my weird imaginary nil:
    NEW_SYM(sym);
    return (CAR(*sym_list_p) = sym);
  }

  for (ae_obj_t * cons = *sym_list_p; cons; cons = CDR(cons)) 
    if (strcmp(string, CAR(cons)->sym_value) == 0) 
      return CAR(cons);
     
  NEW_SYM(sym);
   
  return CAR(*sym_list_p = CONS(sym, *sym_list_p));
}

#undef NEW_SYM
