#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

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
    return_str(AE_FREE____);
  default:
    return "UNRECOGNIZED";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// _unsafe_move method
////////////////////////////////////////////////////////////////////////////////

void ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
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
}

////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(const ae_obj_t * const this) {
  fputs("Cloning          ", stdout);
  ae_obj_put(this);
  putchar('\n');
  fflush(stdout);
  
  ae_obj_t * clone = 0;

#define CLONE_USING_MEMCPY clone = ALLOC_AE_OBJ; memcpy(clone, this, sizeof(ae_obj_t))
#define DUP_C_STR(field) clone->field = strdup(this->field)
  
  switch (this->type) {
  case AE_CONS____:
    clone = ae_obj_map(this, ae_obj_clone);
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
  
  fputs("Cloned           ", stdout);
  ae_obj_put(this);
  fputs(" into ", stdout);
  ae_obj_put(clone);
  putchar('\n');
  fflush(stdout);

  return clone;
}

////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////

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
    if (! this->head)
      fputs("nil", stream);
    else if (! this->tail)
      fprintf(stream, "%011p %-11p %2d",
              this->head,
              this->tail,
              ae_obj_length(this));
    else
      fprintf(stream, "%011p %-011p %2d",
              this->head,
              this->tail,
              ae_obj_length(this));    
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
  case AE_INF_____:
    fputs("∞", stream);
    break;
  case AE_CONS____:
    if (this->type == AE_CONS____ && this->head) {
      LPAR;
      ae_obj_each((ae_obj_t *)this, (ae_obj_each_fun)ae_obj_write);
      BSPC;
      RPAR;
    }
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL__:
    fputs(this->sym_value, stream);
    break;
  case AE_STRING__:
    if (this->str_value == 0) {
      fputs("(null)", stream);
    }
    else {
      DQUO;
      fputs(this->str_value, stream);
      DQUO;
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

size_t ae_obj_length(const ae_obj_t * const this) {
  ASSERT_CONSP(this);
  
  size_t length = 0;
  for (const ae_obj_t * position = this; position; position = position->tail, length++);
  return length;
}

// #define   AE_OBJ_EACH_RECURSES

void ae_obj_each (ae_obj_t * const this, ae_obj_each_fun fun) {
#ifdef    AE_OBJ_EACH_RECURSES
  if (! this) return;

  ASSERT_CONSP(this);

  fun(this->head);

  ae_obj_each(this->tail, fun);
#else  // AE_OBJ_EACH_RECURSES
  ASSERT_CONSP(this);

  for (const ae_obj_t * position = this; position; position = position->tail)
    fun(position->head);
#endif // AE_OBJ_EACH_RECURSES
}

ae_obj_t * ae_obj_map(const ae_obj_t * const this, ae_obj_map_fun fun) {
  if (! this) return 0;
  
  ASSERT_CONSP(this);

  return CONS(fun(this->head), ae_obj_map(this->tail, fun));
}

////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  if (tail)
    ASSERT_CONSP(tail);

  printf("Cons %p %p\n", head, tail);
  fflush(stdout);
  
  ae_obj_t * new_list = NEW_AE_OBJ(AE_CONS____);

  new_list->head = head;
  new_list->tail = tail;

  return new_list;
}

////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////

void ae_obj_push_back(ae_obj_t * const this, ae_obj_t * const obj) {
  ASSERT_CONSP(this);
 
  fputs("Pushing          ", stdout);
  ae_obj_put(obj);
  fputs(" into ", stdout);
  ae_obj_put(this);
  putchar('\n');
  
  if (this->head) {
    ae_obj_t * position = this;
    for (; position->tail; position = position->tail);
    position->tail = NEW_AE_OBJ(AE_CONS____);
    position->tail->head = obj;
  }
  else {
    this->head = obj;
  }

  fputs("Pushed           ", stdout);
  ae_obj_put(obj);
  fputs(" into ", stdout);
  ae_obj_put(this);
  putchar('\n');
  putchar('\n');
}


////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * c_str_intern(char * c_str, ae_obj_t ** sym_list) {
   ae_obj_t * cons = *sym_list;

   printf("Interning %s in ", c_str);
   ae_obj_put(cons);
   putchar(' ');
   ae_obj_write(cons);
   putchar('\n');
   fflush(stdout);
   
   if (! CAR(cons)) {    
     ae_obj_t * sym = NEW_AE_OBJ(AE_SYMBOL__);
     sym->sym_value = strdup(c_str);
     fputs("Created          ", stdout);
     ae_obj_put(sym);
     putchar('\n');

     cons->head = sym;
     printf("Interning returns early with ");
     ae_obj_put(*sym_list);
     putchar(' ');
     ae_obj_write(*sym_list);
     putchar('\n');
   }



   for (; cons; cons = CDR(cons)) {
     fputs("Interning looking at   ", stdout);
     ae_obj_put(cons);
     putchar('\n');
     fflush(stdout);
     
     if (CAR(cons) && strcmp(c_str, CAR(cons)->sym_value) == 0) {
       fputs("Found ", stdout);
       ae_obj_put(CAR(cons));
       fputs(" in ", stdout);
       ae_obj_put(*sym_list);

       putchar('\n');
       fflush(stdout);

       return CAR(cons);
     }
   }

   printf("Interning arrived at %p ", cons);
   putchar('\n');
   fflush(stdout);
   
   ae_obj_t * sym = NEW_AE_OBJ(AE_SYMBOL__);
   sym->sym_value = strdup(c_str);
   fputs("Created          ", stdout);
   ae_obj_put(sym);
   putchar('\n');

//   if (cons) 
     *sym_list = CONS(sym, *sym_list);
     // else
     //(*sym_list)->head = sym;

   fputs("sym_list is      ", stdout);
   ae_obj_put(*sym_list);
   putchar('\n');

   ae_obj_write(*sym_list);
   putchar('\n');

   return CAR((*sym_list));
 }

// micro-lisp's version:
// 
// Object * intern(const char *sym) {
//   Object *_pair = symbols;
//   for ( ; _pair ; _pair = cdr(_pair))
//     if (strncmp(sym, car(_pair)->value.string, TOKEN_MAX)==0)
//       return car(_pair);
//   symbols = cons(newsymbol(sym), symbols);
//   return car(symbols);
// }

////////////////////////////////////////////////////////////////////////////////
// pool 
////////////////////////////////////////////////////////////////////////////////

#ifdef POOL_SIZE
ae_obj_t pool[POOL_SIZE] = { 0 };

ae_obj_t * pool_alloc_ae_obj() {
  for (int ix = POOL_SIZE - 1; ix >= 0; ix--) {
    ae_obj_t * obj = &pool[ix];

    if (obj->type != AE_FREE____)
      continue;
      
#ifdef NOISY_INIT
  fputs("Allocated        ", stdout);
  ae_obj_put(obj);
  putchar('\n');
#endif
  
  return obj;
  }
    
  printf("ERROR: Pool is full.\n");

  assert(0);
  
  return 0;
}

void pool_free_ae_obj(ae_obj_t * const this) {
  ae_obj_init(this, AE_FREE____);
}
#endif
