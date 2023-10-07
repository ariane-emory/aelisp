#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "pool.h"
#include "alist.h"
#include "plist.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Preconditions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(AE_TRACK_ORIGINS_DURING_EVAL) && ! defined(AE_DEBUG_OBJ)
#  error "AE_TRACK_ORIGINS_DURING_EVAL requires AE_DEBUG_OBJ"
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

typedef struct ae_obj_t * (*ae_core_fun)(struct ae_obj_t * const, struct ae_obj_t * const);

struct ae_obj_t; // forward decl.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper macro
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_ESCAPED_CHARACTER(do)                                                             \
  do('a',  '\a')                                                                                   \
  do('b',  '\b')                                                                                   \
  do('f',  '\f')                                                                                   \
  do('n',  '\n')                                                                                   \
  do('r',  '\r')                                                                                   \
  do('t',  '\t')                                                                                   \
  do('v',  '\v')                                                                                   \
  do('\\', '\\')                                                                                   \
  do('\'', '\'')                                                                                   \
  do('\"', '\"')

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_LEXED_TYPE(do)                                                                    \
  do(AE_CHAR)                                                                                      \
  do(AE_CONS)                                                                                      \
  do(AE_CORE)                                                                                      \
  do(AE_ENV)                                                                                       \
  do(AE_ERROR)                                                                                     \
  do(AE_FLOAT)                                                                                     \
  do(AE_INTEGER)                                                                                   \
  do(AE_INVALID)                                                                                   \
  do(AE_LAMBDA)                                                                                    \
  do(AE_MACRO)                                                                                     \
  do(AE_RATIONAL)                                                                                  \
  do(AE_STRING)                                                                                    \
  do(AE_SYMBOL)                                                                                    \

#define enum_entry(x) x,

typedef enum {
  AE_FREE = 0,
  FOR_EACH_LEXED_TYPE(enum_entry)
} ae_type_t;

const char * ae_type_str(const ae_type_t this);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct ae_obj_t {
  // Currently, this field is only used to store an ae_obj_t's type in it's bottom 5 bits, but in 
  // the future it's remaining bits will store other info such as GC related flags:
  unsigned int                metadata;

#ifdef AE_DEBUG_OBJ
  struct ae_obj_t *           debug_data;
#endif
  
  union {
    ae_string_t               str_val;
    ae_string_t               sym_val;
    char                      char_val;
    long long int             int_val;
    double                    float_val;
    struct {
      long long int           numerator_val;
      unsigned long long int  denominator_val;
    }; // when metadata is marked with type AE_RATIONAL
    struct {
      struct ae_obj_t *       head;
      struct ae_obj_t *       tail;
    }; // when metadata is marked with type AE_CONS
    struct {
             ae_string_t      message;
      struct ae_obj_t *       object;
    }; // when metadata is marked with type AE_ERROR
    struct {
      struct ae_obj_t *       symbols;
      struct ae_obj_t *       values;
      struct ae_obj_t *       parent;
    }; // when metadata is marked with type AE_ENV
    struct {
      struct ae_obj_t *       params;
      struct ae_obj_t *       body;
      struct ae_obj_t *       env;
    }; // when metadata is marked with type AE_LAMBDA / AE_MACRO
    struct {
      char                    name[8]; // this name is just for printing purposes.
      bool                    special;
      ae_core_fun             fun_val;
    }; // when metadata is marked with type AE_CORE
  };
}
#ifdef AE_ALIGN_OBJS
#  ifndef AE_OBJ_ALIGNMENT
#    define AE_OBJ_ALIGNMENT 32
#  endif
  __attribute__ ((aligned (AE_OBJ_ALIGNMENT)))
#endif
  ae_obj_t;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_init           (      ae_obj_t * const this,       ae_type_t        type     );
ae_obj_t *    ae_obj_unsafe_move    (      ae_obj_t * const this,       ae_obj_t * const that     );
ae_obj_t *    ae_obj_clone          (      ae_obj_t * const this                                  );
bool          ae_obj_eql            (const ae_obj_t * const this, const ae_obj_t * const that     );
ae_obj_t *    ae_obj_truth          (const bool             this                                  );
ae_type_t     ae_obj_get_type       (const ae_obj_t * const this                                  );
void          ae_obj_set_type       (      ae_obj_t * const this, const ae_type_t        type     );
bool          ae_obj_tailp          (const ae_obj_t * const this                                  );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool          ae_obj_delocalizedp   (const ae_obj_t * const this                                  );
void          ae_obj_set_delocalized(      ae_obj_t * const this, const bool             foo      );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// These two are not yet used and are just here as an example of how to set the next metadata region:
char          ae_obj_get_foo        (const ae_obj_t * const this                                  );
void          ae_obj_set_foo        (      ae_obj_t * const this, const char             foo      );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern ae_obj_t   true_obj;
extern ae_obj_t   nil_obj;
extern ae_obj_t * symbols_list;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ALLOC()                          (pool_alloc_ae_obj())
#define COPY(obj, other)                 (memcpy((obj), (other), sizeof(ae_obj_t)))
#define CLONE(obj)                       (ae_obj_clone((obj)))
#define FREE(obj)                        (pool_free_ae_obj((obj)))
#define INIT(obj, type)                  (ae_obj_init((obj), (type)))
#define MOVE_NEW(other)                  (UNSAFE_MOVE(ALLOC(), other))
#define NEW(type)                        (INIT((ALLOC()), (type)))
#define TRUTH(o)                         (ae_obj_truth((o)))
#define UNSAFE_MOVE(to, from)            (ae_obj_unsafe_move((to), (from)))
#define ZERO(obj)                        (memset((obj), 0, sizeof(ae_obj_t)))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DELOCALIZEDP(o)                  (ae_obj_delocalizedp((o)))
#define MARK_DELOCALIZED(o)              (ae_obj_set_delocalized((o), true))
#define UNMARK_DELOCALIZED(o)            (ae_obj_set_delocalized((o), false))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define CHAR_VAL(obj)                    ((obj)->char_val)
#define DENOM_VAL(obj)                   ((obj)->denominator_val)
#define FLOAT_VAL(obj)                   ((obj)->float_val)
#define CORE_FUN(obj)                    ((obj)->fun_val)
#define INT_VAL(obj)                     ((obj)->int_val)
#define CORE_NAME(obj)                   ((obj)->name)
#define NUMER_VAL(obj)                   ((obj)->numerator_val)
#define STR_VAL(obj)                     ((obj)->str_val)
#define SYM_VAL(obj)                     ((obj)->sym_val)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FUN_PARAMS(obj)                  ((obj)->params)
#define FUN_BODY(obj)                    ((obj)->body)
#define FUN_ENV(obj)                     ((obj)->env)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// These 3 should probably go in a different file but I'm not sure where yet:
#if AE_PREFER_ALIST
#  define KHAS(obj, key)                 (AHAS((obj), (key)))
#  define KGET(obj, key)                 (AGET((obj), (key)))
#  define KSET(obj, key, val)            (ASET((obj), (key), (val)))
#else
#  define KHAS(obj, key)                 (PHAS((obj), (key)))
#  define KGET(obj, key)                 (PGET((obj), (key)))
#  define KSET(obj, key, val)            (PSET((obj), (key), (val)))
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define EMSG(obj)                        ((obj)->message)
#define EOBJ(obj)                        ((obj)->object)
#define EHAS(obj, key)                   (KHAS(EOBJ((obj)), KW(key)))
#define EGET(obj, key)                   (KGET(EOBJ((obj)), KW(key)))
#define ESET(obj, key, val)              (EOBJ((obj)) = (KSET(EOBJ((obj)), KW(key), (val))))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_DEBUG_OBJ
#  define DOBJ(obj)                      ((obj)->debug_data)
#  define DHAS(obj, key)                 (KHAS(DOBJ((obj)), KW(key)))
#  define DGET(obj, key)                 (KGET(DOBJ((obj)), KW(key)))
#  define DSET(obj, key, val)            (DOBJ((obj)) = (KSET(DOBJ((obj)), KW(key), (val))))
#else
#  define DOBJ(obj)                      ((void)obj, NIL)
#  define DHAS(obj, key)                 ((void)obj, (void)key, NIL)
#  define DGET(obj, key)                 ((void)obj, (void)key, NIL)
#  define DSET(obj, key, val)            ((void)obj, (void)key, (void)val, NIL)
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define KW(sym)                          (SYM(":" sym))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPE_STR(t)                      (ae_type_str(((t))) + 3)
#define GET_TYPE(obj)                    (ae_obj_get_type((obj)))
#define SET_TYPE(obj, type)              (ae_obj_set_type((obj), (type)))
#define GET_TYPE_STR(obj)                (TYPE_STR(GET_TYPE((obj))))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NIL                              (&nil_obj)
#define TRUE                             (&true_obj)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQL(obj, other)                  (ae_obj_eql((obj), (other)))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unary predicates
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPE_PREDICATE(obj, type)        ((obj) && (GET_TYPE((obj)) == type))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(obj)                       (! CONSP(obj))
#define CHARP(obj)                       TYPE_PREDICATE((obj), AE_CHAR)
#define CONSP(obj)                       (TYPE_PREDICATE((obj), AE_CONS))
#define COREP(obj)                       (TYPE_PREDICATE((obj), AE_CORE))
#define ENVP(obj)                        TYPE_PREDICATE((obj), AE_ENV)
#define ERRORP(obj)                      TYPE_PREDICATE((obj), AE_ERROR)
#define FLOATP(obj)                      TYPE_PREDICATE((obj), AE_FLOAT)
#define FREEP(obj)                       TYPE_PREDICATE((obj), AE_FREE)
#define INTEGERP(obj)                    TYPE_PREDICATE((obj), AE_INTEGER)
#define INVALIDP(obj)                    TYPE_PREDICATE((obj), AE_INVALID)
#define LAMBDAP(obj)                     TYPE_PREDICATE((obj), AE_LAMBDA)
#define MACROP(obj)                      (TYPE_PREDICATE((obj), AE_MACRO))
#define RATIONALP(obj)                   TYPE_PREDICATE((obj), AE_RATIONAL)
#define STRINGP(obj)                     TYPE_PREDICATE((obj), AE_STRING)
#define SYMBOLP(obj)                     (TYPE_PREDICATE((obj), AE_SYMBOL))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// can't update COREP_INTERNAL fore some reason yet:
#define SPECIALP(obj)                    ({ CAPTURE((obj)); MACROP(CAPTURED) || (COREP(CAPTURED) && CAPTURED->special); })
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define KEYWORDP(obj)                    ({ CAPTURE((obj)); SYMBOLP(CAPTURED) ? CAPTURED->sym_val[0] == ':' : false; })
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TRUEP(obj)                       ((obj) == TRUE)
#define NILP(obj)                        ((obj) == NIL)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TAILP(obj)                       (ae_obj_tailp(obj))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CHAR(val)                                                                                                  \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_CHAR);                                                                                       \
CHAR_VAL  (_obj) = (val);                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CONS(head, tail)                                                                                           \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_CONS);                                                                                       \
CAR       (_obj) = (head);                                                                                             \
CDR       (_obj) = (tail);                                                                                             \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CORE(name, val, _special)                                                                                  \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_CORE);                                                                                       \
CORE_FUN  (_obj) = (val);                                                                                              \
strcpy(CORE_NAME(_obj), name);                                                                                         \
_obj->special = _special;                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ERROR(obj, msg)                                                                                            \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_ERROR);                                                                                      \
EMSG   (_obj) = (msg);                                                                                                 \
EOBJ   (_obj) = (obj);                                                                                                 \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_FLOAT(val)                                                                                                 \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_FLOAT);                                                                                      \
FLOAT_VAL (_obj) = (val);                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_INT(val)                                                                                                   \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_INTEGER);                                                                                    \
INT_VAL   (_obj) = (val);                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_LAMBDA(params_, body_, env_)                                                                               \
({                                                                                                                     \
ae_obj_t * _obj = NEW(AE_LAMBDA);                                                                                      \
_obj->params    = params_;                                                                                             \
_obj->body      = body_;                                                                                               \
_obj->env       = env_;                                                                                                \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_MACRO(params_, body_, env_)                                                                                \
({                                                                                                                     \
ae_obj_t * _obj = NEW(AE_MACRO);                                                                                       \
_obj->params    = params_;                                                                                             \
_obj->body      = body_;                                                                                               \
_obj->env       = env_;                                                                                                \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_RATIONAL(numer, denom)                                                                                     \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_RATIONAL);                                                                                   \
NUMER_VAL (_obj) = (numer);                                                                                            \
DENOM_VAL (_obj) = (denom);                                                                                            \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_STRING(val)                                                                                                \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_STRING);                                                                                     \
STR_VAL   (_obj) = (val);                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_SYMBOL(val)                                                                                                \
({                                                                                                                     \
ae_obj_t * _obj  = NEW(AE_SYMBOL);                                                                                     \
SYM_VAL   (_obj) = (val);                                                                                              \
_obj;                                                                                                                  \
})
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
