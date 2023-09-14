#pragma once

////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper
////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_ESCAPED_CHARACTER(DO)                                                                                                      \
  DO('a',  '\a')                                                                                                                            \
  DO('b',  '\b')                                                                                                                            \
  DO('f',  '\f')                                                                                                                            \
  DO('n',  '\n')                                                                                                                            \
  DO('r',  '\r')                                                                                                                            \
  DO('t',  '\t')                                                                                                                            \
  DO('v',  '\v')                                                                                                                            \
  DO('\\', '\\')                                                                                                                            \
  DO('\'', '\'')                                                                                                                            \
  DO('\"', '\"')                                                                                                                            \
  DO('\?', '\?')                                                                                                                         

////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_LEXED_TYPE(DO)                                                                                                            \
  DO(AE_INTEGER_)                                                                                                                           \
  DO(AE_INVALID_)                                                                                                                           \
  DO(AE_CHAR____)                                                                                                                           \
  DO(AE_FLOAT___)                                                                                                                           \
  DO(AE_CONS____)                                                                                                                           \
  DO(AE_LPAREN__)                                                                                                                           \
  DO(AE_RPAREN__)                                                                                                                           \
  DO(AE_QUOTE___)                                                                                                                           \
  DO(AE_RATIONAL)                                                                                                                           \
  DO(AE_INF_____)                                                                                                                           \
  DO(AE_STRING__)                                                                                                                           \
  DO(AE_SYMBOL__)                                                                                                                             

#define enum_node(x) x,

typedef enum {
  AE_FREE____ = 0,
  FOR_EACH_LEXED_TYPE(enum_node)
} ae_type_t;

#define INTEGERP(o)         ((o)->type == AE_INTEGER_)
#define INVALIDP(o)         ((o)->type == AE_INVALID_)
#define CHARP(o)            ((o)->type == AE_CHAR____)
#define FLOATP(o)           ((o)->type == AE_FLOAT___)
#define CONSP(o)            ((o)->type == AE_CONS____)
#define LPARENP(o)          ((o)->type == AE_LPAREN__)
#define RPARENP(o)          ((o)->type == AE_RPAREN__)
#define QUOTEP(o)           ((o)->type == AE_QUOTE___)
#define RATIONALP(o)        ((o)->type == AE_RATIONAL)
#define INFP(o)             ((o)->type == AE_INF_____)
#define STRINGP(o)          ((o)->type == AE_STRING__)
#define SYMBOLP(o)          ((o)->type == AE_SYMBOL__)
#define ASSERT_INTEGERP(o)  (assert(INTEGERP(o)))
#define ASSERT_INVALIDP(o)  (assert(INVALIDP(o)))
#define ASSERT_CHARP(o)     (assert(CHARP(o)))
#define ASSERT_FLOATP(o)    (assert(FLOATP(o)))
#define ASSERT_CONSP(o)     (assert((CONSP(o))))
#define ASSERT_LPARENP(o)   (assert(LPARENP(o)))
#define ASSERT_RPARENP(o)   (assert(RPARENP(o)))
#define ASSERT_QUOTEP(o)    (assert(QUOTEP(o)))
#define ASSERT_RATIONALP(o) (assert(RATIONALP(o)))
#define ASSERT_INFP(o)      (assert(INFP(o)))
#define ASSERT_STRINGP(o)   (assert(STRINGP(o)))
#define ASSERT_SYMBOLP(o)   (assert(SYMBOLP(o)))

const char * ae_type_str(const ae_type_t this);

struct ae_obj_t;

typedef void              (*ae_obj_each_fun)(struct ae_obj_t * const);
typedef struct ae_obj_t * (*ae_obj_map_fun)(struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_obj_t {
  ae_type_t        type;
  union {
    ae_string_t         str_value;
    ae_string_t         sym_value;
    char                char_value;
    int                 int_value;
    double              float_value;
    struct {
      int               numerator_value;
      unsigned int      denominator_value;
    };
    struct {
      struct ae_obj_t * head;
      struct ae_obj_t * tail;
    };
  };
} ae_obj_t;

//------------------------------------------------------------------------------
// Obj'ss methods
//------------------------------------------------------------------------------

ae_obj_t *    ae_obj_init        (      ae_obj_t * const this,  ae_type_t type);
ae_obj_t *    ae_obj_unsafe_move (      ae_obj_t * const this,  ae_obj_t * const that);
ae_obj_t *    ae_obj_clone       (const ae_obj_t * const this);

// ugly putses:
void          ae_obj_fput        (const ae_obj_t * const this,  FILE * stream);
void          ae_obj_put         (const ae_obj_t * const this);
// byte-oriented putses:
void          ae_obj_fput_bytes  (const ae_obj_t * const this,  FILE * stream);
void          ae_obj_put_bytes   (const ae_obj_t * const this);
// write:
void          ae_obj_fwrite      (const ae_obj_t * const this,  FILE * stream);
void          ae_obj_write       (const ae_obj_t * const this);

// For AE_CONS____es:
size_t        ae_obj_length      (const ae_obj_t * const this);
void          ae_obj_push_back   (      ae_obj_t * const this, ae_obj_t * const obj);
void          ae_obj_each        (      ae_obj_t * const this, ae_obj_each_fun fun);
ae_obj_t *    ae_obj_map         (      ae_obj_t * const this, ae_obj_map_fun fun);

// This returns a new obj:
ae_obj_t *    ae_obj_cons        (      ae_obj_t * const head, ae_obj_t * const tail);

////////////////////////////////////////////////////////////////////////////////
// Intern
////////////////////////////////////////////////////////////////////////////////

ae_obj_t * c_str_intern(char * c_str, ae_obj_t ** const sym_list_p);

////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////

#define ALLOC                 (pool_alloc_ae_obj())
#define NEW(type)             (ae_obj_init(ALLOC, (type)))
#define MOVE_NEW(that)        (ae_obj_unsafe_move(ALLOC, (that)))
#define PUSH_BACK(this, that) (ae_obj_push_back((this), (that)))
#define CAR(obj)              ((obj)->head)
#define CDR(obj)              ((obj)->tail)
#define CONS(head, tail)      (ae_obj_cons((head), (tail)))

////////////////////////////////////////////////////////////////////////////////
// pool
////////////////////////////////////////////////////////////////////////////////

#define POOL_SIZE (1 << 7)

extern ae_obj_t               pool[POOL_SIZE];
extern const ae_obj_t * const pool_first;
extern const ae_obj_t * const pool_last;

ae_obj_t * pool_alloc_ae_obj();
void       pool_free_ae_obj(ae_obj_t * const this);
void pool_print(void);
void pool_clear(void);
