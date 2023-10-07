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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Preconditions
////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(AE_TRACK_ORIGINS_DURING_EVAL) && ! defined(AE_DEBUG_OBJ)
#  error "AE_TRACK_ORIGINS_DURING_EVAL requires AE_DEBUG_OBJ"
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

typedef struct ae_obj_t * (*ae_core_fun)(struct ae_obj_t * const, struct ae_obj_t * const);

struct ae_obj_t; // forward decl.

////////////////////////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper macro
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_init           (      ae_obj_t * const this,       ae_type_t        type     );
ae_obj_t *    ae_obj_unsafe_move    (      ae_obj_t * const this,       ae_obj_t * const that     );
ae_obj_t *    ae_obj_clone          (      ae_obj_t * const this                                  );
bool          ae_obj_eql            (const ae_obj_t * const this, const ae_obj_t * const that     );
ae_obj_t *    ae_obj_truth          (const bool             this                                  );
ae_type_t     ae_obj_get_type       (const ae_obj_t * const this                                  );
void          ae_obj_set_type       (      ae_obj_t * const this, const ae_type_t        type     );
////////////////////////////////////////////////////////////////////////////////////////////////////
bool          ae_obj_get_delocalized(const ae_obj_t * const this                                  );
void          ae_obj_set_delocalized(      ae_obj_t * const this, const bool             foo      );
////////////////////////////////////////////////////////////////////////////////////////////////////
// These two are not yet used and are just here as an example of how to set the next metadata region:
char          ae_obj_get_foo        (const ae_obj_t * const this                                  );
void          ae_obj_set_foo        (      ae_obj_t * const this, const char             foo      );
////////////////////////////////////////////////////////////////////////////////////////////////////

extern ae_obj_t   true_obj;
extern ae_obj_t   nil_obj;
extern ae_obj_t * symbols_list;

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ALLOC()                      (pool_alloc_ae_obj())
#define COPY(this, that)             (memcpy((this), (that), sizeof(ae_obj_t)))
#define CLONE(this)                  (ae_obj_clone((this)))
#define FREE(this)                   (pool_free_ae_obj((this)))
#define INIT(this, type)             (ae_obj_init((this), (type)))
#define MOVE_NEW(that)               (UNSAFE_MOVE(ALLOC(), that))
#define NEW(type)                    (INIT((ALLOC()), (type)))
#define TRUTH(o)                     (ae_obj_truth((o)))
#define UNSAFE_MOVE(to, from)        (ae_obj_unsafe_move((to), (from)))
#define ZERO(this)                   (memset((this), 0, sizeof(ae_obj_t)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define MARKED_AS_DELOCALIZEDP(o)    (ae_obj_get_delocalized((o)))
#define MARK_DELOCALIZED(o)          (ae_obj_set_delocalized((o), true))
#define UNMARK_DELOCALIZED(o)        (ae_obj_set_delocalized((o), false))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CHAR_VAL(this)               ((this)->char_val)
#define DENOM_VAL(this)              ((this)->denominator_val)
#define FLOAT_VAL(this)              ((this)->float_val)
#define CORE_FUN(this)               ((this)->fun_val)
#define INT_VAL(this)                ((this)->int_val)
#define CORE_NAME(this)              ((this)->name)
#define NUMER_VAL(this)              ((this)->numerator_val)
#define STR_VAL(this)                ((this)->str_val)
#define SYM_VAL(this)                ((this)->sym_val)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FUN_PARAMS(this)             ((this)->params)
#define FUN_BODY(this)               ((this)->body)
#define FUN_ENV(this)                ((this)->env)
////////////////////////////////////////////////////////////////////////////////////////////////////
// These 3 should probably go in a different file but I'm not sure where yet:
#if AE_PREFER_ALIST
#  define KHAS(this, key)            (AHAS((this), (key)))
#  define KGET(this, key)            (AGET((this), (key)))
#  define KSET(this, key, val)       (ASET((this), (key), (val)))
#else
#  define KHAS(this, key)            (PHAS((this), (key)))
#  define KGET(this, key)            (PGET((this), (key)))
#  define KSET(this, key, val)       (PSET((this), (key), (val)))
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EMSG(this)                   ((this)->message)
#define EOBJ(this)                   ((this)->object)
#define EHAS(this, key)              (KHAS(EOBJ((this)), KW(key)))
#define EGET(this, key)              (KGET(EOBJ((this)), KW(key)))
#define ESET(this, key, val)         (EOBJ((this)) = (KSET(EOBJ((this)), KW(key), (val))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_DEBUG_OBJ
#  define DOBJ(this)                 ((this)->debug_data)
#  define DHAS(this, key)            (KHAS(DOBJ((this)), KW(key)))
#  define DGET(this, key)            (KGET(DOBJ((this)), KW(key)))
#  define DSET(this, key, val)       (DOBJ((this)) = (KSET(DOBJ((this)), KW(key), (val))))
#else
#  define DOBJ(this)                 ((void)this, NIL)
#  define DHAS(this, key)            ((void)this, (void)key, NIL)
#  define DGET(this, key)            ((void)this, (void)key, NIL)
#  define DSET(this, key, val)       ((void)this, (void)key, (void)val, NIL)
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////
#define KW(sym)                      (SYM(":" sym))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPE_STR(t)                  (ae_type_str(((t))) + 3)
#define GET_TYPE(this)               (ae_obj_get_type((this)))
#define SET_TYPE(this, type)         (ae_obj_set_type((this), (type)))
#define GET_TYPE_STR(o)              (TYPE_STR(GET_TYPE((o))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NIL                          (&nil_obj)
#define TRUE                         (&true_obj)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQ(this, that)               ((this) && (that) && ((this)) == ((that)))
#define EQL(this, that)              ((this) && (that) && (ae_obj_eql((this), (that))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CAPTURE(o)                   const ae_obj_t * const tmp_##__LINE__ = (o)
#define CAPTURED                     tmp_##__LINE__
////////////////////////////////////////////////////////////////////////////////////////////////////
// The new version of SPECIALP is commented out because it causes problems I haven't gotten around
// to diagnosing yet:
#define SPECIALP(o)                  ((MACROP ((o))) || (COREP   ((o)) && ((o)->special)))
// #define SPECIALP(o)               ({ CAPTURE(o); MACROP(CAPTURED) || (COREP(CAPTURED) && CAPTURED->special); })
////////////////////////////////////////////////////////////////////////////////////////////////////
// The new version of SPECIALP is commented out because it causes problems I haven't gotten around
// to diagnosing yet:
#define KEYWORDP(o)                  ((SYMBOLP((o))) && (SYM_VAL ((o)))[0] == ':')
// #define KEYWORDP(o)               ({ CAPTURE(o); SYMBOLP(CAPTURED) && SYM_VAL(CAPTURED)[0] == ':'; })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NILP(o)                      ({ CAPTURE(o); (CAPTURED) && (CAPTURED == NIL);  })
#define TRUEP(o)                     ({ CAPTURE(o); (CAPTURED) && (CAPTURED == TRUE); })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPE_PREDICATE(type, o)      ({ CAPTURE(o); (CAPTURED) && GET_TYPE(CAPTURED) == type; })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(o)                     ({ CAPTURE(o); !CONSP(CAPTURED); })
#define CHARP(o)                     TYPE_PREDICATE(AE_CHAR, o)
#define CONSP(o)                     TYPE_PREDICATE(AE_CONS, o)
#define COREP(o)                     TYPE_PREDICATE(AE_CORE, o)
#define ENVP(o)                      TYPE_PREDICATE(AE_ENV, o)
#define ERRORP(o)                    TYPE_PREDICATE(AE_ERROR, o)
#define FLOATP(o)                    TYPE_PREDICATE(AE_FLOAT, o)
#define FREEP(o)                     TYPE_PREDICATE(AE_FREE, o)
#define INTEGERP(o)                  TYPE_PREDICATE(AE_INTEGER, o)
#define INVALIDP(o)                  TYPE_PREDICATE(AE_INVALID, o)
#define LAMBDAP(o)                   TYPE_PREDICATE(AE_LAMBDA, o)
#define MACROP(o)                    TYPE_PREDICATE(AE_MACRO, o)
#define RATIONALP(o)                 TYPE_PREDICATE(AE_RATIONAL, o)
#define STRINGP(o)                   TYPE_PREDICATE(AE_STRING, o)
#define SYMBOLP(o)                   TYPE_PREDICATE(AE_SYMBOL, o)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CHAR(val)                                                                              \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CHAR);                                                                   \
CHAR_VAL  (_obj) = (val);                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CONS(head, tail)                                                                       \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CONS);                                                                   \
CAR       (_obj) = (head);                                                                         \
CDR       (_obj) = (tail);                                                                         \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CORE(name, val, _special)                                                              \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CORE);                                                                   \
CORE_FUN  (_obj) = (val);                                                                          \
strcpy(CORE_NAME(_obj), name);                                                                     \
_obj->special = _special;                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ERROR(obj, msg)                                                                        \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_ERROR);                                                                  \
EMSG   (_obj) = (msg);                                                                             \
EOBJ   (_obj) = (obj);                                                                             \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_FLOAT(val)                                                                             \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_FLOAT);                                                                  \
FLOAT_VAL (_obj) = (val);                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_INT(val)                                                                               \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_INTEGER);                                                                \
INT_VAL   (_obj) = (val);                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_LAMBDA(params_, body_, env_)                                                           \
({                                                                                                 \
ae_obj_t * _obj = NEW(AE_LAMBDA);                                                                  \
_obj->params    = params_;                                                                         \
_obj->body      = body_;                                                                           \
_obj->env       = env_;                                                                            \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_MACRO(params_, body_, env_)                                                            \
({                                                                                                 \
ae_obj_t * _obj = NEW(AE_MACRO);                                                                   \
_obj->params    = params_;                                                                         \
_obj->body      = body_;                                                                           \
_obj->env       = env_;                                                                            \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_RATIONAL(numer, denom)                                                                 \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_RATIONAL);                                                               \
NUMER_VAL (_obj) = (numer);                                                                        \
DENOM_VAL (_obj) = (denom);                                                                        \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_STRING(val)                                                                            \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_STRING);                                                                 \
STR_VAL   (_obj) = (val);                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_SYMBOL(val)                                                                            \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_SYMBOL);                                                                 \
SYM_VAL   (_obj) = (val);                                                                          \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
