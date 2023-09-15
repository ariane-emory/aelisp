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
  PUT(this);
  putchar('\n');
#endif

  ZERO(this);
  TYPE(this) = type;

#ifdef NOISY_INIT
  fputs("Initialized      ", stdout);
  PUT(this);
  putchar('\n');
#endif

  return this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _unsafe_move method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
  ASSERT_NEQ(this, that);
  
#ifdef NOISY_INIT
  fputs("Moving           ", stdout);
  PUT(that);
  fputs(" to ", stdout);
  PUT(this);
  putchar('\n');
#endif

  COPY(this, that);
  INIT(that, AE_FREE____);

#ifdef NOISY_INIT
  fputs("Moved            ", stdout);
  PUT(that);
  fputs(" to ", stdout);
  PUT(this);
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
  PUT(this);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * clone = NULL;

#define CLONE_USING_MEMCPY clone = ALLOC(); memcpy(clone, this, sizeof(ae_obj_t))
#define DUP_C_STR(field) clone->field = strdup(this->field)
  
  switch (TYPE(this)) {
  case AE_CONS____:
    clone = MAP(this, ae_obj_clone);
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
  PUT(this);
  fputs(" into ", stdout);
  PUT(clone);
  putchar('\n');
  fflush(stdout);
#endif

  return clone;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "%011p[ %s ", this, TYPE_STR(TYPE(this)));
  
  switch (TYPE(this)) {
  case AE_LPAREN__:
  case AE_RPAREN__:
  case AE_INVALID_:
  case AE_FREE____:
  case AE_INF_____:
    BSPC; break;
  case AE_CONS____:
    if      (NULLP(CAR(this)))
      fputs("nil", stream);
    else if (NULLP(CDR(this)))
      fprintf(stream, "%011p %-11p %2d",  CAR(this), CDR(this), 666); // LENGTH(this));
    else
      fprintf(stream, "%011p %-011p %2d", CAR(this), CDR(this), 666); // LENGTH(this));    
    break;
  case AE_SYMBOL__:
  case AE_STRING__:
  case AE_CHAR____:
  case AE_FLOAT___:
  case AE_INTEGER_:
  case AE_RATIONAL:
    FWRITE(this, stream);
    BSPC;
    break;
  default:
    LSQR;
    FWRITE(this, stream);
    BSPC;
    RSQR;
  }
  SPC;
  RSQR;
}

void ae_obj_put(const ae_obj_t * const this) {
  FPUT(this, stdout);
}

#define MEMSTREAM(buff, stream)                                                                                                             \
  char * buff;                                                                                                                              \
  size_t size;                                                                                                                              \
  FILE * stream = open_memstream(&buff, &size);

char * ae_obj_sput(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_obj_fput(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
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

char * ae_obj_sput_bytes(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_obj_fput_bytes(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_write(const ae_obj_t * const this) {
  FWRITE(this, stdout);
}

static FILE * stream;

static void ae_obj_fwrite_internal(const ae_obj_t * const this) {
  switch (TYPE(this)) {
  case AE_INF_____:
    fputs("∞", stream);
    break;
  case AE_CONS____:
    if (CONSP(this) && CAR(this) ) {
      fputc('(', stream);
      EACH((ae_obj_t *)this, ae_obj_fwrite_internal);
      fputc('\b', stream);
      fputc(')', stream);
    }
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL__:
    fputs(SYM_VAL(this), stream);
    break;
  case AE_STRING__:
    if (STR_VAL(this) == NULL) {
      int wrote = fputs("(null)", stream);
      for (; wrote < 40; wrote++)
        SPC;
    }
    else {
      fputc('"', stream);
      fputs(STR_VAL(this), stream);
      fputc('"', stream);
    }
    break;
  case AE_INTEGER_:
    fprintf(stream, "%d", this->int_value);
    break;
  case AE_RATIONAL:
    fprintf(stream, "%d/%d", this->numerator_value, this->denominator_value);
    break;
  case AE_FLOAT___:
    fprintf(stream, "%g", this->float_value);
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

    fputc('\'', stream);
    fputs(tmp, stream);
    fputc('\'', stream);
    
    break;
  }
  default:
    fprintf(stream, "UNPRINTABLE");
  }
  
  fputc(' ', stream);
}

void ae_obj_fwrite(const ae_obj_t * const this, FILE * stream_) {
  stream = stream_;
  ae_obj_fwrite_internal(this);
}

char * ae_obj_swrite(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_obj_fwrite(this, stream_);
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _map method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_list_map(const ae_obj_t * const list, ae_list_map_fun fun) {
  if (! list)
    return NULL;
  
  ASSERT_CONSP(list);

  return CONS(fun(CAR(list) ), MAP(CDR(list), fun));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// other methods
////////////////////////////////////////////////////////////////////////////////////////////////////

size_t ae_list_length(const ae_obj_t * const list) {
  ASSERT_CONSP(list);
  
  if (! CAR(list) ) return 0;

  size_t length = 0;

  FOR_EACH(elem, list)
    length++;

  return length;
}

void ae_list_each (ae_obj_t * const list, ae_list_each_fun fun) {
  ASSERT_CONSP(list);

  for (const ae_obj_t * position = list; position; position = CDR(position)) {
    fun(CAR(position) );
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_list_has_member(const ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);

   for (const ae_obj_t * position = list; position; position = CDR(position))
    if (EQ(CAR(position), member))
      return true;
  
   return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_list_remove_member(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);

  DECL(new_list,  NULL);
  
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
  ASSERT_NEQ(head, tail); // consing an obj onto itself is not yet supported.
  
  if (tail)
    ASSERT_CONSP(tail);

#ifdef NOISY_INIT
  printf("Cons %p %p\n", head, tail);
  fflush(stdout);
#endif
  
  DECL(new_list, NEW(AE_CONS____));

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
    DECL(position, list);
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

#define NEW_SYM(sym) ae_obj_t * sym = NEW(AE_SYMBOL__); SYM_VAL(sym) = strdup(string)

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
