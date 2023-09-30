#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ae_pool.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

struct ae_obj_t; // forward decl.
typedef struct ae_obj_t * (*ae_core_fun)(struct ae_obj_t * const, struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper macro
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_ESCAPED_CHARACTER(DO)                                                             \
  DO('a',  '\a')                                                                                   \
  DO('b',  '\b')                                                                                   \
  DO('f',  '\f')                                                                                   \
  DO('n',  '\n')                                                                                   \
  DO('r',  '\r')                                                                                   \
  DO('t',  '\t')                                                                                   \
  DO('v',  '\v')                                                                                   \
  DO('\\', '\\')                                                                                   \
  DO('\'', '\'')                                                                                   \
  DO('\"', '\"')

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_LEXED_TYPE(DO)                                                                    \
  DO(AE_CHAR)                                                                                      \
  DO(AE_CONS)                                                                                      \
  DO(AE_CORE)                                                                                      \
  DO(AE_ENV)                                                                                       \
  DO(AE_ERROR)                                                                                     \
  DO(AE_FLOAT)                                                                                     \
  DO(AE_INTEGER)                                                                                   \
  DO(AE_INVALID)                                                                                   \
  DO(AE_LAMBDA)                                                                                    \
  DO(AE_MACRO)                                                                                     \
  DO(AE_RATIONAL)                                                                                  \
  DO(AE_STRING)                                                                                    \
  DO(AE_SYMBOL)                                                                                    \

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
ae_obj_t *    ae_obj_init          (      ae_obj_t * const this,       ae_type_t        type      );
ae_obj_t *    ae_obj_unsafe_move   (      ae_obj_t * const this,       ae_obj_t * const that      );
ae_obj_t *    ae_obj_clone         (      ae_obj_t * const this                                   );
bool          ae_obj_eql           (const ae_obj_t * const this, const ae_obj_t * const that      );
ae_obj_t *    ae_obj_truth         (const bool             this                                   );
ae_type_t     ae_obj_get_type      (const ae_obj_t * const this                                   );
void          ae_obj_set_type      (      ae_obj_t * const this, const ae_type_t        type      );
////////////////////////////////////////////////////////////////////////////////////////////////////
// These two are not yet used and are just here as an example of how to set the next metadata region:
char          ae_obj_get_foo       (const ae_obj_t * const this                                   );
void          ae_obj_set_foo       (      ae_obj_t * const this, const char             foo       );
////////////////////////////////////////////////////////////////////////////////////////////////////

extern ae_obj_t   true_obj;
extern ae_obj_t   nil_obj;
extern ae_obj_t * symbols_list;

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define COPY(this, that)        (memcpy((this), (that), sizeof(ae_obj_t)))
#define ALLOC()                 (pool_alloc_ae_obj())
#define CLONE(this)             (ae_obj_clone((this)))
#define FREE(this)              (pool_free_ae_obj((this)))
#define INIT(this, type)        (ae_obj_init((this), (type)))
#define MOVE_NEW(that)          (UNSAFE_MOVE(ALLOC(), that))
#define NEW(type)               (INIT((ALLOC()), (type)))
#define UNSAFE_MOVE(to, from)   (ae_obj_unsafe_move((to), (from)))
#define TRUTH(o)                (ae_obj_truth((o)))
#define ZERO(this)              (memset((this), 0, sizeof(ae_obj_t)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CHAR_VAL(this)          ((this)->char_val)
#define DENOM_VAL(this)         ((this)->denominator_val)
#define FLOAT_VAL(this)         ((this)->float_val)
#define CORE_FUN(this)          ((this)->fun_val)
#define INT_VAL(this)           ((this)->int_val)
#define CORE_NAME(this)         ((this)->name)
#define NUMER_VAL(this)         ((this)->numerator_val)
#define STR_VAL(this)           ((this)->str_val)
#define SYM_VAL(this)           ((this)->sym_val)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ERR_MSG(this)           ((this)->message)
#define ERR_OBJ(this)           ((this)->object)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define FUN_PARAMS(this)        ((this)->params)
#define FUN_BODY(this)          ((this)->body)
#define FUN_ENV(this)           ((this)->env)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define GET_TYPE(this)          (ae_obj_get_type((this)))
#define SET_TYPE(this, type)    (ae_obj_set_type((this), (type)))
#define TYPE_STR(o)             (ae_type_str(GET_TYPE((o))) + 3)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQ(this, that)          ((! NULLP((this))) && (! NULLP((that))) && ((this)) == ((that)))
#define NEQ(this, that)         ((! NULLP((this))) && (! NULLP((that))) && ((this)) != ((that)))
#define EQL(this, that)         ((! NULLP((this))) && (! NULLP((that))) && (ae_obj_eql((this), (that))))
#define NEQL(this, that)        ((! NULLP((this))) && (! NULLP((that))) && (! EQL((this), (that))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(o)                ((! NULLP((o))) && (! CONSP((o))))
#define NULLP(o)                (! (o))
#define SPECIALP(o)             ((MACROP ((o))) || (COREP   ((o)) && ((o)->special)))
#define KEYWORDP(o)             ((SYMBOLP((o))) && (SYM_VAL ((o)))[0] == ':')
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NIL                     (&nil_obj)
#define TRUE                    (&true_obj)
#define NILP(o)                 ((! NULLP((o))) && ((o) == NIL))
#define TRUEP(o)                ((! NULLP((o))) && ((o) == TRUE))
////////////////////////////////////////////////////////////////////////////////////////////////////
/* This could probably be DRYed up by using an X macro to create function versions of them, but   */
/* their names would be ugly unless we renamed the types first to remove the AE_ prefix.          */
/* Undecided.                                                                                     */
#define CHARP(o)                ((! NULLP((o))) && (GET_TYPE((o))  == AE_CHAR))
#define CONSP(o)                ((! NULLP((o))) && (GET_TYPE((o))  == AE_CONS))
#define COREP(o)                ((! NULLP((o))) && (GET_TYPE((o))  == AE_CORE))
#define ERRORP(o)               ((! NULLP((o))) && (GET_TYPE((o))  == AE_ERROR))
#define FLOATP(o)               ((! NULLP((o))) && (GET_TYPE((o))  == AE_FLOAT))
#define FREEP(o)                ((! NULLP((o))) && (GET_TYPE((o))  == AE_FREE))
#define INTEGERP(o)             ((! NULLP((o))) && (GET_TYPE((o))  == AE_INTEGER))
#define INVALIDP(o)             ((! NULLP((o))) && (GET_TYPE((o))  == AE_INVALID))
#define LAMBDAP(o)              ((! NULLP((o))) && (GET_TYPE((o))  == AE_LAMBDA))
#define MACROP(o)               ((! NULLP((o))) && (GET_TYPE((o))  == AE_MACRO))
#define RATIONALP(o)            ((! NULLP((o))) && (GET_TYPE((o))  == AE_RATIONAL))
#define STRINGP(o)              ((! NULLP((o))) && (GET_TYPE((o))  == AE_STRING))
#define SYMBOLP(o)              ((! NULLP((o))) && (GET_TYPE((o))  == AE_SYMBOL))
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
ERR_MSG   (_obj) = (msg);                                                                          \
ERR_OBJ   (_obj) = (obj);                                                                          \
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

