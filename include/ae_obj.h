#pragma once

#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////
// Escaped chars helper
////////////////////////////////////////////////////////////////////////////////

#define FOR_ESCAPED_CHARACTER_DO(DO)                                                                                                        \
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

#define FOR_LEXED_TYPES_DO(DO)                                                                                                              \
  DO(AE_CHAR)                                                                                                                               \
  DO(AE_FLOAT)                                                                                                                              \
  DO(AE_INTEGER)                                                                                                                            \
  DO(AE_LIST)                                                                                                                               \
  DO(AE_PAREN)                                                                                                                              \
  DO(AE_QUOTE)                                                                                                                              \
  DO(AE_RATIONAL)                                                                                                                           \
  DO(AE_STRING)                                                                                                                             \
  DO(AE_SYMBOL)                                                                                                                             

#define enum_node(x) x,

typedef enum {
  AE_FREE = 0,
  AE_INVALID,
  FOR_LEXED_TYPES_DO(enum_node)
} ae_type_t;

const char * ae_type_str(const ae_type_t this);

/////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t;

typedef void   (*ae_node_each_fun)(struct ae_obj_t * const);

typedef struct ae_node_t {
  struct ae_obj_t *    head;
  struct ae_node_t *   tail;
} ae_node_t;

typedef ae_node_t * ae_list_t;

//------------------------------------------------------------------------------

void               ae_node_init     (      ae_node_t * const this);
size_t             ae_node_length   (const ae_node_t * const this);
void               ae_node_push_back(      ae_node_t * const this, struct ae_obj_t * const obj);
void               ae_node_each     (      ae_node_t * const this, ae_node_each_fun fun);
ae_node_t *        ae_node_create   (struct ae_obj_t * const obj);

////////////////////////////////////////////////////////////////////////////////
// Obj struct
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_obj_t {
  ae_type_t                 type;
  union {
    ae_string_t             str_value;
    ae_string_t             sym_value;
    char                    char_value;
    int                     int_value;
    double                  float_value;
    ae_node_t               list_value;
    struct {
      int                   numerator_value;
      unsigned int          denominator_value;
    };

  };
} ae_obj_t;

//------------------------------------------------------------------------------
// Obj's methods
//------------------------------------------------------------------------------

void          ae_obj_init        (      ae_obj_t * const this,  ae_type_t type);
void          ae_obj_unsafe_move (      ae_obj_t * const this,  ae_obj_t * const that);
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


