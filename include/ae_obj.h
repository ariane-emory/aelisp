#pragma once

#include <stdbool.h>

#include <ae_obj_pool.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define DECL(name, this)        ae_obj_t * name = (this);
#define SET(name, this)         name = (this);
#define COPY(this, that)        (memcpy((this), (that), sizeof(ae_obj_t)))
#define ZERO(this)              (memset((this), 0, sizeof(ae_obj_t)))

#define ALLOC()                 (pool_alloc_ae_obj())
#define CADR(this)              (CAR(CDR(this)))
#define CAR(this)               ((this)->head)
#define CDR(this)               ((this)->tail)
#define CLONE(this)             (ae_obj_clone((this)))
#define CONS(head, tail)        (ae_obj_cons((head), (tail)))
#define EACH(this, fun)         (ae_list_each(this, (ae_list_each_fun)fun))
#define FPUT(this, stream)      (ae_obj_fput((this), (stream)))
#define FREE()                  (pool_free_ae_obj())
#define FWRITE(this, stream)    (ae_obj_fwrite((this), (stream)))
#define INIT(this, type)        (ae_obj_init((this), (type)))
#define INTERN(sym_list, str)   (ae_list_intern_string((sym_list), (str)))
#define LENGTH(this)            (ae_list_length(this))
#define MAP(this, fun)          (ae_list_map(this, (ae_list_map_fun)fun))
#define MEMBER(this, that)      (ae_list_has_member((this), (that)))
#define MOVE_NEW(that)          (ae_obj_unsafe_move((ALLOC()), (that)))
#define NEW(type)               (ae_obj_init((ALLOC()), (type)))
#define PUSH(this, that)        (ae_list_push_back((this), (that)))
#define PUT(this)               (ae_obj_put(this))
#define REMOVE(list, elem)      (ae_list_remove_member(list, elem))
#define SWRITE(this)            (ae_obj_swrite(this))
#define STR_VAL(this)           ((this)->str_value)
#define SYM_VAL(this)           ((this)->sym_value)
#define TYPE(this)              ((this)->type)
#define TYPE_STR(type)          (ae_type_str((type)))
#define UNSAFE_MOVE(to, from)   (ae_obj_unsafe_move((to), (from)))
#define WRITE(this)             (ae_obj_write(this))

#define EQ(this, that)          ((this) == (that))
#define NEQ(this, that)         ((this) != (that))

#define ATOMP(o)                ((o)->type >= AE_INVALID_)
#define CHARP(o)                ((o)->type == AE_CHAR____)
#define CONSP(o)                ((o)->type == AE_CONS____)
#define FLOATP(o)               ((o)->type == AE_FLOAT___)
#define FREEP(o)                ((o)->type == AE_FREE____)
#define INTEGERP(o)             ((o)->type == AE_INTEGER_)
#define INVALIDP(o)             ((o)->type == AE_INVALID_)
#define LPARENP(o)              ((o)->type == AE_LPAREN__)
#define QUOTEP(o)               ((o)->type == AE_QUOTE___)
#define RATIONALP(o)            ((o)->type == AE_RATIONAL)
#define RPARENP(o)              ((o)->type == AE_RPAREN__)
#define STRINGP(o)              ((o)->type == AE_STRING__)
#define SYMBOLP(o)              ((o)->type == AE_SYMBOL__)
#define NULLP(o)                (!(o))

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
#define ASSERT_NULlP(o)         (assert(NULLP(o)))

#define FOR_EACH(elem, list)    for (const ae_obj_t * position = (list), * elem = CAR(position); position; position = ((void)(elem), CDR(position)))
  

////////////////////////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper macro
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_LEXED_TYPE(DO)                                                                                                             \
  DO(AE_CONS____)                                                                                                                           \
  DO(AE_INVALID_)                                                                                                                           \
  DO(AE_CHAR____)                                                                                                                           \
  DO(AE_INTEGER_)                                                                                                                           \
  DO(AE_RATIONAL)                                                                                                                           \
  DO(AE_FLOAT___)                                                                                                                           \
  DO(AE_INF_____)                                                                                                                           \
  DO(AE_STRING__)                                                                                                                           \
  DO(AE_SYMBOL__)                                                                                                                           \
  DO(AE_LPAREN__)                                                                                                                           \
  DO(AE_RPAREN__)                                                                                                                           \
  DO(AE_QUOTE___)

#define enum_node(x) x,

typedef enum {
  AE_FREE____ = 0,
  FOR_EACH_LEXED_TYPE(enum_node)
} ae_type_t;

const char * ae_type_str(const ae_type_t this);

struct ae_obj_t;

typedef void              (*ae_list_each_fun)(struct ae_obj_t * const);
typedef struct ae_obj_t * (*ae_list_map_fun )(struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct ae_obj_t {
  ae_type_t             type;
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
} ae_obj_t; // __attribute__((packed)) ae_obj_t;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_init          (      ae_obj_t *  const this,       ae_type_t         type    );
ae_obj_t *    ae_obj_unsafe_move   (      ae_obj_t *  const this,       ae_obj_t *  const that    );
ae_obj_t *    ae_obj_clone         (const ae_obj_t *  const this                                  );
// writing / printing methods //////////////////////////////////////////////////////////////////////
void          ae_obj_write         (const ae_obj_t *  const this                                  );
void          ae_obj_put           (const ae_obj_t *  const this                                  );
void          ae_obj_put_bytes     (const ae_obj_t *  const this                                  );
char *        ae_obj_swrite        (const ae_obj_t *  const this                                  );
char *        ae_obj_sput          (const ae_obj_t *  const this                                  );
char *        ae_obj_sput_bytes    (const ae_obj_t *  const this                                  );
void          ae_obj_fwrite        (const ae_obj_t *  const this,       FILE *            stream  );
void          ae_obj_fput          (const ae_obj_t *  const this,       FILE *            stream  );
void          ae_obj_fput_bytes    (const ae_obj_t *  const this,       FILE *            stream  );
// list-relater methods ////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_list_intern_string(      ae_obj_t ** const sym_list_p, ae_string_t       string  );
ae_obj_t *    ae_obj_cons          (      ae_obj_t *  const head,       ae_obj_t *  const tail    );
ae_obj_t *    ae_list_push_back    (      ae_obj_t *  const list,       ae_obj_t *  const member  );
ae_obj_t *    ae_list_remove_member(      ae_obj_t *  const list,       ae_obj_t *  const member  );
bool          ae_list_has_member   (const ae_obj_t *  const list,       ae_obj_t *  const member  );
size_t        ae_list_length       (const ae_obj_t *  const list                                  );
ae_obj_t *    ae_list_map          (const ae_obj_t *  const list,       ae_list_map_fun   fun     );
void          ae_list_each         (      ae_obj_t *  const list,       ae_list_each_fun  fun     );
////////////////////////////////////////////////////////////////////////////////////////////////////

