#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "capture.h"
#include "plist.h"
#include "pool.h"

extern bool log_eval;
extern bool log_core;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef char * ae_string_t;
typedef struct ae_obj_t * (*ae_core_fun)(struct ae_obj_t * const, struct ae_obj_t * const, int args_length);
struct ae_obj_t; // forward decl.
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper macro
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_ESCAPED_CHARACTER(DO)                                                                                                \
  DO('a',  '\a')                                                                                                                      \
  DO('b',  '\b')                                                                                                                      \
  DO('f',  '\f')                                                                                                                      \
  DO('n',  '\n')                                                                                                                      \
  DO('r',  '\r')                                                                                                                      \
  DO('t',  '\t')                                                                                                                      \
  DO('v',  '\v')                                                                                                                      \
  DO('\\', '\\')                                                                                                                      \
  DO('\'', '\'')                                                                                                                      \
  DO('\"', '\"')
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types enum
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH_LEXED_TYPE(DO)                                                                                                       \
  DO(AE_CHAR)                                                                                                                         \
  DO(AE_CONS)                                                                                                                         \
  DO(AE_CORE)                                                                                                                         \
  DO(AE_ENV)                                                                                                                          \
  DO(AE_ERROR)                                                                                                                        \
  DO(AE_FLOAT)                                                                                                                        \
  DO(AE_INTEGER)                                                                                                                      \
  DO(AE_INVALID)                                                                                                                      \
  DO(AE_LAMBDA)                                                                                                                       \
  DO(AE_MACRO)                                                                                                                        \
  DO(AE_RATIONAL)                                                                                                                     \
  DO(AE_STRING)                                                                                                                       \
  DO(AE_SYMBOL)
//======================================================================================================================
#define enum_entry(x) x,
//======================================================================================================================
typedef enum {
  AE_FREE = 0,
  FOR_EACH_LEXED_TYPE(enum_entry)
} ae_type_t;
//======================================================================================================================
const char * ae_type_str(const ae_type_t this);
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj struct
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct ae_obj_t {
  unsigned long long int      metadata;

  struct ae_obj_t *           properties;
  
  union {
    ae_string_t               str_val;
    ae_string_t               sym_val;
    ae_string_t               error_message;
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
      ae_string_t             name;
      bool                    special;
      ae_core_fun             fun_val;
    }; // when metadata is marked with type AE_CORE
  };
}
//======================================================================================================================
#ifdef AE_ALIGN_OBJS
#  ifndef AE_OBJ_ALIGNMENT
#    define AE_OBJ_ALIGNMENT 32
#  endif
  __attribute__ ((aligned (AE_OBJ_ALIGNMENT)))
#endif
//======================================================================================================================
  ae_obj_t;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_init                  (      ae_obj_t * const this,       ae_type_t        type     );
ae_obj_t *    ae_obj_unsafe_move           (      ae_obj_t * const this,       ae_obj_t * const that     );
ae_obj_t *    ae_obj_clone                 (      ae_obj_t * const this                                  );
bool          ae_obj_eql                   (const ae_obj_t * const this, const ae_obj_t * const that     );
ae_obj_t *    ae_obj_truth                 (const bool             this                                  );
ae_type_t     ae_obj_get_type              (const ae_obj_t * const this                                  );
void          ae_obj_set_type              (      ae_obj_t * const this, const ae_type_t        type     );
//======================================================================================================================
bool          ae_obj_settable_symbolp      (const ae_obj_t * const this                                  );
bool          ae_obj_keywordp              (const ae_obj_t * const this                                  );
bool          ae_obj_special_funp          (const ae_obj_t * const this                                  );
bool          ae_obj_special_symp          (const ae_obj_t * const this                                  );
bool          ae_obj_tailp                 (const ae_obj_t * const this                                  );
//======================================================================================================================
bool          ae_obj_get_delocalized       (const ae_obj_t * const this                                  );
void          ae_obj_set_delocalized       (      ae_obj_t * const this, const bool             foo      );
//======================================================================================================================
// These two are not yet used and are just here as an example of how to set the next metadata region:
char          ae_obj_get_foo               (const ae_obj_t * const this                                  );
void          ae_obj_set_foo               (      ae_obj_t * const this, const char             foo      );
//======================================================================================================================
unsigned int  ae_obj_get_min_args          (const ae_obj_t * const this                                  );
void          ae_obj_set_min_args          (      ae_obj_t * const this, const unsigned int      min_args);
//======================================================================================================================
unsigned int  ae_obj_get_max_args          (const ae_obj_t * const this                                  );
void          ae_obj_set_max_args          (      ae_obj_t * const this, const unsigned int      max_args);
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// data
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern ae_obj_t   true_obj;
extern ae_obj_t   nil_obj;
extern ae_obj_t * symbols_list;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ALLOC                            pool_alloc_ae_obj
#define COPY(obj, other)                 (memcpy((obj), (other), sizeof(ae_obj_t)))
#define CLONE                            ae_obj_clone
#define FREE                             pool_free_ae_obj
#define INIT                             ae_obj_init
#define MOVE_NEW(other)                  (UNSAFE_MOVE(ALLOC(), other))
#define NEW(type)                        (INIT((ALLOC()), (type)))
#define TRUTH                            ae_obj_truth
#define UNSAFE_MOVE                      ae_obj_unsafe_move
#define ZERO(obj)                        (memset((obj), 0, sizeof(ae_obj_t)))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define DELOCALIZEDP                     ae_obj_get_delocalized
#define MARK_DELOCALIZED(o)              (ae_obj_set_delocalized((o), true))
#define UNMARK_DELOCALIZED(o)            (ae_obj_set_delocalized((o), false))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define CHAR_VAL(obj)                    ((obj)->char_val)
#define DENOM_VAL(obj)                   ((obj)->denominator_val)
#define FLOAT_VAL(obj)                   ((obj)->float_val)
#define CORE_FUN(obj)                    ((obj)->fun_val)
#define INT_VAL(obj)                     ((obj)->int_val)
#define CORE_NAME(obj)                   ((obj)->name)
#define NUMER_VAL(obj)                   ((obj)->numerator_val)
#define STR_VAL(obj)                     ((obj)->str_val)
#define SYM_VAL(obj)                     ((obj)->sym_val)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FUN_PARAMS(obj)                  ((obj)->params)
#define FUN_BODY(obj)                    ((obj)->body)
#define FUN_ENV(obj)                     ((obj)->env)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define EMSG(obj)                        ((obj)->error_message)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_MIN_ARGS                    ae_obj_get_min_args
#define CORE_MAX_ARGS                    ae_obj_get_max_args
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ASSIGN_PROPS(props, obj)         ((obj)->properties = (props))
#define PROPS(obj)                       ((obj)->properties)
#define HAS_PROP(key, obj)               (PHAS(PROPS((obj)), KW(key)))
#define HAS_PROP_RAW(key, obj)           (PHAS(PROPS((obj)), (key)))
#define GET_PROP(key, obj)               (PGET(PROPS((obj)), KW(key)))
#define GET_PROP_RAW(key, obj)           (PGET(PROPS((obj)), (key))) 
#define PUT_PROP(val, key, obj)          ({ CAPTURE(obj); PROPS(CAPTURED) = (PSET_INTERNAL(PROPS(CAPTURED), KW(key), (val))); })
#define PUT_PROP_RAW(val, key, obj)      ({ CAPTURE(obj); PROPS(CAPTURED) = (PSET_INTERNAL(PROPS(CAPTURED),   (key), (val))); })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define KW(sym)                          (SYM(":" sym))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPE_STR(t)                      (ae_type_str(((t))) + 3)
#define GET_TYPE                         ae_obj_get_type
#define SET_TYPE                         ae_obj_set_type
#define GET_TYPE_STR(obj)                (TYPE_STR(GET_TYPE((obj))))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQL                              ae_obj_eql
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unary predicates
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define TYPEP(obj, type)                 ((GET_TYPE((obj)) == type))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(obj)                       (! CONSP(obj))
#define CHARP(obj)                       (TYPEP((obj), AE_CHAR))
#define CONSP(obj)                       (TYPEP((obj), AE_CONS))
#define COREP(obj)                       (TYPEP((obj), AE_CORE))
#define ENVP(obj)                        (TYPEP((obj), AE_ENV))
#define ERRORP(obj)                      (TYPEP((obj), AE_ERROR))
#define FLOATP(obj)                      (TYPEP((obj), AE_FLOAT))
#define FREEP(obj)                       (TYPEP((obj), AE_FREE))
#define INTEGERP(obj)                    (TYPEP((obj), AE_INTEGER))
#define INVALIDP(obj)                    (TYPEP((obj), AE_INVALID))
#define LAMBDAP(obj)                     (TYPEP((obj), AE_LAMBDA))
#define MACROP(obj)                      (TYPEP((obj), AE_MACRO))
#define RATIONALP(obj)                   (TYPEP((obj), AE_RATIONAL))
#define STRINGP(obj)                     (TYPEP((obj), AE_STRING))
#define SYMBOLP(obj)                     (TYPEP((obj), AE_SYMBOL))
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define SETTABLEP                        ae_obj_settable_symbolp
#define KEYWORDP                         ae_obj_keywordp
#define SPECIAL_FUNP                     ae_obj_special_funp
#define SPECIAL_SYMP                     ae_obj_special_symp
#define TAILP                            ae_obj_tailp
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NILP(obj)                        ((obj) == NIL)
#define TRUEP(obj)                       ((obj) == TRUE)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NIL                              (&nil_obj)
#define TRUE                             (&true_obj)
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Constructor macros
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CHAR(val)                                                                                                                 \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_CHAR);                                                                                                    \
  CHAR_VAL  (_obj) = (val);                                                                                                           \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CONS(head, tail)                                                                                                          \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_CONS);                                                                                                    \
  CAR       (_obj) = (head);                                                                                                          \
  CDR       (_obj) = (tail);                                                                                                          \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_CORE(name, val, _special, min_args, max_args)                                                                             \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_CORE);                                                                                                    \
  CORE_FUN  (_obj) = (val);                                                                                                           \
  char * const new_name = free_list_malloc(strlen(name) + 1);                                                                         \
  strcpy(new_name, name);                                                                                                             \
  CORE_NAME(_obj) = new_name;                                                                                                         \
  _obj->special = _special;                                                                                                           \
  ae_obj_set_min_args(_obj, min_args);                                                                                                \
  ae_obj_set_max_args(_obj, max_args);                                                                                                \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ENV(parent_, symbols_, values_)                                                                                           \
  ({                                                                                                                                  \
  ae_obj_t * _obj = NEW(AE_ENV);                                                                                                      \
  _obj->parent    = (parent_);                                                                                                        \
  _obj->symbols   = (symbols_);                                                                                                       \
  _obj->values    = (values_);                                                                                                        \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_ERROR(...)                                                                                                                \
  ({                                                                                                                                  \
  char * const tmp = free_list_malloc(256);                                                                                           \
  snprintf(tmp, 256, __VA_ARGS__);                                                                                                    \
  char * const err_msg = free_list_malloc(strlen(tmp) + 1);                                                                           \
  strcpy(err_msg, tmp);                                                                                                               \
  free_list_free(tmp);                                                                                                                \
  ae_obj_t * _obj = NEW(AE_ERROR);                                                                                                    \
  EMSG(_obj) = err_msg;                                                                                                               \
  /* log_eval = true;  */                                                                                                             \
  /* log_core = true; */                                                                                                              \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_FLOAT(val)                                                                                                                \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_FLOAT);                                                                                                   \
  FLOAT_VAL (_obj) = (val);                                                                                                           \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_INT(val)                                                                                                                  \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_INTEGER);                                                                                                 \
  INT_VAL   (_obj) = (val);                                                                                                           \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_LAMBDA(params_, body_, env_)                                                                                              \
  ({                                                                                                                                  \
  ae_obj_t * _obj = NEW(AE_LAMBDA);                                                                                                   \
  _obj->params    = params_;                                                                                                          \
  _obj->body      = CONS(SYM("progn"),  body_);                                                                                       \
  _obj->env       = env_;                                                                                                             \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_MACRO(params_, body_, env_)                                                                                               \
  ({                                                                                                                                  \
  ae_obj_t * _obj = NEW(AE_MACRO);                                                                                                    \
  _obj->params    = params_;                                                                                                          \
  _obj->body      = CONS(SYM("progn"),  body_);                                                                                       \
  _obj->env       = env_;                                                                                                             \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_RATIONAL(numer, denom)                                                                                                    \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_RATIONAL);                                                                                                \
  NUMER_VAL (_obj) = (numer);                                                                                                         \
  DENOM_VAL (_obj) = (denom);                                                                                                         \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_STRING(val)                                                                                                               \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_STRING);                                                                                                  \
  STR_VAL   (_obj) = (val);                                                                                                           \
  /* printf("new string: %s\n", STR_VAL(_obj)); */                                                                                    \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_SYMBOL(val)                                                                                                               \
  ({                                                                                                                                  \
  ae_obj_t * _obj  = NEW(AE_SYMBOL);                                                                                                  \
  SYM_VAL   (_obj) = (val);                                                                                                           \
  _obj;                                                                                                                               \
  })
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


/* Local Variables: */
/* c-syntactic-indentation-in-macros: nil */
/* c-backslash-column: 134 */
/* End: */
