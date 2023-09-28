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
typedef struct ae_obj_t * (*ae_core_fun)(struct ae_obj_t * const);

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
  DO(AE_CONS)                                                                                      \
  DO(AE_SYMBOL)                                                                                    \
  DO(AE_STRING)                                                                                    \
  DO(AE_CHAR)                                                                                      \
  DO(AE_INTEGER)                                                                                   \
  DO(AE_RATIONAL)                                                                                  \
  DO(AE_FLOAT)                                                                                     \
  DO(AE_INF)                                                                                       \
  DO(AE_QUOTE)                                                                                     \
  DO(AE_ENV)                                                                                       \
  DO(AE_LAMBDA)                                                                                    \
  DO(AE_MACRO)                                                                                     \
  DO(AE_CORE)                                                                                      \
  DO(AE_INVALID)

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
  // Currently, this field is only used to store an ae_obj_t in it's bottom 5 bits, but in the future
  // it's remaining bits will store other info such as GC related flags:
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
    }; // AE_RATIONAL
    struct {
      struct ae_obj_t *       head;
      struct ae_obj_t *       tail;
    }; // AE_CONS
    struct {
      struct ae_obj_t *       symbols;
      struct ae_obj_t *       values;
      struct ae_obj_t *       parent;
    }; // AE_ENV
    struct {
      struct ae_obj_t *       params;
      struct ae_obj_t *       body;
      struct ae_obj_t *       env;
    }; // AE_LAMBDA / AE_MACRO
    struct {
      char                    name[8]; // this name is just for printing purposes.
      bool                    special;
      ae_core_fun             fun_val;
    }; // AE_CORE
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
#define FREE(this)              (pool_free_ae_obj((thia)))
#define INIT(this, type)        (ae_obj_init((this), (type)))
#define MOVE_NEW(that)          (UNSAFE_MOVE(ALLOC(), that))
#define NEW(type)               (INIT((ALLOC()), (type)))
#define UNSAFE_MOVE(to, from)   (ae_obj_unsafe_move((to), (from)))
#define TRUTH(o)                (ae_obj_truth((o)))
#define ZERO(this)              (memset((this), 0, sizeof(ae_obj_t)))
#define TYPE_STR(o)             (ae_type_str(GET_TYPE((o))) + 3)
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
#define FUN_PARAMS(this)        ((this)->params)
#define FUN_BODY(this)          ((this)->body)
#define FUN_ENV(this)           ((this)->env)
////////////////////////////////////////////////////////////////////////////////////////////////////
#define GET_TYPE(this)          (ae_obj_get_type((this)))
#define SET_TYPE(this, type)    (ae_obj_set_type((this), (type)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NULLP(o)                (! (o))
#define NOT_NULLP(o)            (! NULLP(o))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQ(this, that)          (NOT_NULLP((this)) && NOT_NULLP((that)) && ((this)) == ((that)))
#define NEQ(this, that)         (NOT_NULLP((this)) && NOT_NULLP((that)) && ((this)) != ((that)))
#define EQL(this, that)         (NOT_NULLP((this)) && NOT_NULLP((that)) && (ae_obj_eql((this), (that))))
#define NEQL(this, that)        (NOT_NULLP((this)) && NOT_NULLP((that)) && (! EQL((this), (that))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(o)                (NOT_NULLP((o)) && (! CONSP((o))))
#define CHARP(o)                (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_CHAR))
#define CONSP(o)                (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_CONS))
#define COREP(o)                (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_CORE))
#define FLOATP(o)               (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_FLOAT))
#define FREEP(o)                (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_FREE))
#define INTEGERP(o)             (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_INTEGER))
#define INVALIDP(o)             (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_INVALID))
#define KEYWORDP(o)             (SYMBOLP  ((o)) && SYM_VAL((o))[0] == ':')
#define LAMBDAP(o)              (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_LAMBDA))
#define MACROP(o)               (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_MACRO))
#define QUOTEP(o)               (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_QUOTE))
#define RATIONALP(o)            (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_RATIONAL))
#define STRINGP(o)              (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_STRING))
#define SYMBOLP(o)              (NOT_NULLP((o)) && (GET_TYPE((o))  == AE_SYMBOL))
#define FUNP(o)                 (LAMBDAP  ((o)) || COREP((o)))
#define SPECIALP(o)             (MACROP((o)) || (COREP((o)) && ((o)->special)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ERROR                   (SYM(":ERROR"))
#define NIL                     (&nil_obj)
#define NILP(o)                 (NOT_NULLP((o)) && ((o) == NIL))
#define NOT_NILP(o)             (! NILP((o)))
#define TRUE                    (&true_obj)
#define TRUEP(o)                (NOT_NULLP((o)) && ((o) == TRUE))
#define NOT_TRUEP(o)            (! TRUEP((o)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CONS(head, tail)                                                                       \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CONS);                                                                   \
CAR       (_obj) = (head);                                                                         \
CDR       (_obj) = (tail);                                                                         \
_obj;                                                                                              \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CHAR(val)                                                                              \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CHAR);                                                                   \
CHAR_VAL  (_obj) = (val);                                                                          \
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
#define NEW_CHAR(val)                                                                              \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CHAR);                                                                   \
CHAR_VAL  (_obj) = (val);                                                                          \
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
#define NEW_CORE(name, val, _special)                                                              \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_CORE);                                                                   \
CORE_FUN  (_obj) = (val);                                                                          \
strcpy(CORE_NAME(_obj), name);                                                                     \
_obj->special = _special;                                                                          \
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
#define NEW_RATIONAL(numer, denom)                                                                 \
({                                                                                                 \
ae_obj_t * _obj  = NEW(AE_RATIONAL);                                                               \
NUMER_VAL (_obj) = (numer);                                                                        \
DENOM_VAL (_obj) = (denom);                                                                        \
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

/* #include "ae_list.h" */
/* #include "ae_write.h" */
/* #include "ae_env.h" */
