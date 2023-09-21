#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj_pool.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char  *             ae_string_t;

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define COPY(this, that)        (memcpy((this), (that), sizeof(ae_obj_t)))
#define ZERO(this)              (memset((this), 0, sizeof(ae_obj_t)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ALLOC()                 (pool_alloc_ae_obj())
#define CLONE(this)             (ae_obj_clone((this)))
#define DENOM_VAL(this)         ((this)->denominator_val)
#define FLOAT_VAL(this)         ((this)->float_val)
#define FREE(this)              (pool_free_ae_obj((thia)))
#define INIT(this, type)        (ae_obj_init((this), (type)))
#define INT_VAL(this)           ((this)->int_val)
#define MOVE_NEW(that)          (ae_obj_unsafe_move((ALLOC()), (that)))
#define NEW(type)               (ae_obj_init((ALLOC()), (type)))
#define NUMER_VAL(this)         ((this)->numerator_val)
#define STR_VAL(this)           ((this)->str_val)
#define SYM_VAL(this)           ((this)->sym_val)
#define TYPE_STR(type)          (ae_type_str((type)))
#define UNSAFE_MOVE(to, from)   (ae_obj_unsafe_move((to), (from)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define GET_TYPE(this)          (ae_obj_get_type((this)))
#define SET_TYPE(this, type)    (ae_obj_set_type((this), (type)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NULLP(o)                (! (o))
#define NOT_NULLP(o)            (! NULLP(o))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EQ(this, that)          (NOT_NULLP((this)) && NOT_NULLP((that)) && ((this)) == ((that)))
#define NEQ(this, that)         (NOT_NULLP((this)) && NOT_NULLP((that)) && ((this)) != ((that)))
#define EQL(this, that)         (NOT_NULLP((this)) && NOT_NULLP((that)) && (ae_obj_equal((this), (that))))
#define NEQL(this, that)        (NOT_NULLP((this)) && NOT_NULLP((that)) && (! EQL((this), (that))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ATOMP(o)                (NOT_NULLP((o)) && (! CONSP((o))))
#define CHARP(o)                (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_CHAR))
#define CONSP(o)                (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_CONS))
#define FLOATP(o)               (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_FLOAT))
#define FREEP(o)                (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_FREE))
#define INTEGERP(o)             (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_INTEGER))
#define INVALIDP(o)             (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_INVALID))
#define QUOTEP(o)               (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_QUOTE))
#define RATIONALP(o)            (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_RATIONAL))
#define STRINGP(o)              (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_STRING))
#define SYMBOLP(o)              (NOT_NULLP((o)) && (GET_TYPE((o)) == AE_SYMBOL))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ASSERT_EQ(this, that)   (assert(EQ((this), (that))))
#define ASSERT_NEQ(this, that)  (assert(NEQ((this), (that))))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define ASSERT_ATOMP(o)         (assert(ATOMP(o))
#define ASSERT_CHARP(o)         (assert(CHARP(o)))
#define ASSERT_CONSP(o)         (assert((CONSP(o))))
#define ASSERT_FLOATP(o)        (assert(FLOATP(o)))
#define ASSERT_FREEP(o)         (assert(FREEP(o)))
#define ASSERT_INFP(o)          (assert(INFP(o)))
#define ASSERT_INTEGERP(o)      (assert(INTEGERP(o)))
#define ASSERT_INVALIDP(o)      (assert(INVALIDP(o)))
#define ASSERT_QUOTEP(o)        (assert(QUOTEP(o)))
#define ASSERT_RATIONALP(o)     (assert(RATIONALP(o)))
#define ASSERT_STRINGP(o)       (assert(STRINGP(o)))
#define ASSERT_SYMBOLP(o)       (assert(SYMBOLP(o)))
#define ASSERT_NULLP(o)         (assert(NULLP(o)))
#define ASSERT_NOT_NULLP(o)     (assert(NOT_NULLP(o)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NILP(o)                 (NOT_NULLP((o)) && ((o) == NIL))
#define NIL                     (&nil_obj)
#define TRU                     (&true_obj)

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
  DO('\"', '\"')                                                                                   \
  DO('\?', '\?')                                                                                                                         

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
  // Currently, this field is only used to store an ae_obj_t in it's bottom 4 bits, but in the future
  // it's remaining bits will store other info such as GC related flags:
  unsigned int          metadata;

#define AE_TYPE_BITS  4
#define AE_FOOO_BITS  8

  union {
    ae_string_t              str_val;
    ae_string_t              sym_val;
    char                     char_val;
    long long int            int_val;
    double                   float_val;
    struct {
      long long int          numerator_val;
      unsigned long long int denominator_val;
    }; // AE_RATIONAL
    struct {
      struct ae_obj_t *      head;
      struct ae_obj_t *      tail;
    }; // AE_CONS
    struct {
      struct ae_obj_t *      parent;
      struct ae_obj_t *      symbols;
      struct ae_obj_t *      values;
    }; // AE_ENV
    struct {
      struct ae_obj_t *      params;
      struct ae_obj_t *      body;
      struct ae_obj_t *      env;
    }; // AE_LAMBDA
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
bool          ae_obj_equal         (const ae_obj_t * const this, const ae_obj_t * const that      );
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

#include "ae_obj_list.h"
#include "ae_obj_write.h"
#include "ae_obj_env.h"
