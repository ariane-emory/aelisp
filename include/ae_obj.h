#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ae_obj_preconditions.h"
#include "ae_obj_pool.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char  *             ae_string_t;
typedef void              (*ae_list_each_fun)(struct ae_obj_t *  const);
typedef struct ae_obj_t * (*ae_list_map_fun )(const struct ae_obj_t *  const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define COPY(this, that)        (memcpy((this), (that), sizeof(ae_obj_t)))
#define ZERO(this)              (memset((this), 0, sizeof(ae_obj_t)))

#define ALLOC()                 (pool_alloc_ae_obj())
#define CLONE(this)             (ae_obj_clone((this)))
#define DENOM_VAL(this)         ((this)->denominator_val)
#define FLOAT_VAL(this)         ((this)->float_val)
#define FREE()                  (pool_free_ae_obj())
#define INIT(this, type)        (ae_obj_init((this), (type)))
#define INT_VAL(this)           ((this)->int_val)
#define MOVE_NEW(that)          (ae_obj_unsafe_move((ALLOC()), (that)))
#define NEW(type)               (ae_obj_init((ALLOC()), (type)))
#define NUMER_VAL(this)         ((this)->numerator_val)
#define STR_VAL(this)           ((this)->str_val)
#define SYM_VAL(this)           ((this)->sym_val)
#define TYPE(this)              ((this)->type)
#define TYPE_STR(type)          (ae_type_str((type)))
#define UNSAFE_MOVE(to, from)   (ae_obj_unsafe_move((to), (from)))

#define EQ(this, that)          ((this) == (that))
#define NEQ(this, that)         (! EQ((this), (that)))
#define EQL(this, that)         (ae_obj_equal((this), (that)))
#define NEQL(this, that)        (! EQL((this), (that)))

#define ATOMP(o)                ((o)->type >= AE_INVALID)
#define CHARP(o)                ((o)->type == AE_CHAR)
#define CONSP(o)                ((o)->type == AE_CONS)
#define FLOATP(o)               ((o)->type == AE_FLOAT)
#define FREEP(o)                ((o)->type == AE_FREE)
#define INTEGERP(o)             ((o)->type == AE_INTEGER)
#define INVALIDP(o)             ((o)->type == AE_INVALID)
#define LPARENP(o)              ((o)->type == AE_LPAREN)
#define QUOTEP(o)               ((o)->type == AE_QUOTE)
#define RATIONALP(o)            ((o)->type == AE_RATIONAL)
#define RPARENP(o)              ((o)->type == AE_RPAREN)
#define STRINGP(o)              ((o)->type == AE_STRING)
#define SYMBOLP(o)              ((o)->type == AE_SYMBOL)
#define NULLP(o)                (! (o))

#define ASSERT_EQ(this, that)   (assert((this) == (that)))
#define ASSERT_NEQ(this, that)  (assert((this) != (that)))

#define ASSERT_ATOMP(o)         (assert(ATOMP(o))
#define ASSERT_CHARP(o)         (assert(CHARP(o)))
#define ASSERT_CONSP(o)         (assert((CONSP(o))))
#define ASSERT_FLOATP(o)        (assert(FLOATP(o)))
#define ASSERT_FREEP(o)         (assert(FREEP(o)))
#define ASSERT_INFP(o)          (assert(INFP(o)))
#define ASSERT_INTEGERP(o)      (assert(INTEGERP(o)))
#define ASSERT_INVALIDP(o)      (assert(INVALIDP(o)))
#define ASSERT_LPARENP(o)       (assert(LPARENP(o)))
#define ASSERT_QUOTEP(o)        (assert(QUOTEP(o)))
#define ASSERT_RATIONALP(o)     (assert(RATIONALP(o)))
#define ASSERT_RPARENP(o)       (assert(RPARENP(o)))
#define ASSERT_STRINGP(o)       (assert(STRINGP(o)))
#define ASSERT_SYMBOLP(o)       (assert(SYMBOLP(o)))
#define ASSERT_NULLP(o)         (assert(NULLP(o)))

#ifdef NIL_IS_AN_INTERNED_SYMBOL
#  define NIL                   (INTERN(&symbol_list, "nil"))
#  define NILP(o)               ((o) == NIL)
#  define TRU                   (INTERN(&symbol_list, "t"))
#endif

#if defined(NIL_IS_AN_UNINTERNED_SYMBOL) || defined(NIL_IS_IMPLICIT)
#  define NIL                   (&nil_obj)
#  define NILP(o)               ((o) == NIL)
#  define TRU                   (&true_obj)
#endif

#ifdef NIL_EXISTS
#  define CDR_IS_NILP(o)        (NILP(CDR((o))))
#else
#  define CDR_IS_NILP(o)        (NULLP(CDR(())))
#endif

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
  DO(AE_INVALID)                                                                                   \
  DO(AE_CHAR)                                                                                      \
  DO(AE_INTEGER)                                                                                   \
  DO(AE_RATIONAL)                                                                                  \
  DO(AE_FLOAT)                                                                                     \
  DO(AE_INF)                                                                                       \
  DO(AE_STRING)                                                                                    \
  DO(AE_SYMBOL)                                                                                    \
  DO(AE_LPAREN)                                                                                    \
  DO(AE_RPAREN)                                                                                    \
  DO(AE_QUOTE)

#define enum_node(x) x,

typedef enum {
  AE_FREE = 0,
  FOR_EACH_LEXED_TYPE(enum_node)
} ae_type_t;

const char * ae_type_str(const ae_type_t this);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct ae_obj_t {
  ae_type_t             type;
  union {
    ae_string_t         str_val;
    ae_string_t         sym_val;
    char                char_val;
    int                 int_val;
    double              float_val;
    struct {
      int               numerator_val;
      unsigned int      denominator_val;
    };
    struct {
      struct ae_obj_t * head;
      struct ae_obj_t * tail;
    };
  };
} ae_obj_t; // __attribute__((packed)) ae_obj_t;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_init          (      ae_obj_t *  const this,  ae_type_t         type         );
ae_obj_t *    ae_obj_unsafe_move   (      ae_obj_t *  const this,  ae_obj_t *  const that         );
ae_obj_t *    ae_obj_clone         (      ae_obj_t *  const this                                  );
bool          ae_obj_equal         (const ae_obj_t *  const this,  const ae_obj_t *  const that   );
ae_obj_t *    ae_obj_truth         (const bool              this                                  );
////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(NIL_IS_AN_UNINTERNED_SYMBOL) || defined(NIL_IS_IMPLICIT)
extern ae_obj_t true_obj;
extern ae_obj_t nil_obj;
#endif

#ifdef NIL_IS_AN_INTERNED_SYMBOL
extern ae_obj_t * symbol_list;
#endif

#include "ae_obj_list.h"
#include "ae_obj_write.h"
