#pragma once

#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////

typedef char * ae_string_t;

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

const char * const ae_type_str(const ae_type_t this);

////////////////////////////////////////////////////////////////////////////////
// Object struct
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_object_t {
  char *                    c_str;
  ae_type_t                 type;
  union {
    ae_string_t             string_value;
    char                    char_value;
    int                     int_value;
    double                  float_value;
    ae_list_t               list_value;
    struct {
      int                   numerator_value;
      unsigned int          denominator_value;
    };

  };
} ae_object_t;

//------------------------------------------------------------------------------

void               ae_object_init(ae_object_t * const this);
const char * const ae_object_str(const ae_object_t * const this);
void               ae_object_move(ae_object_t * const this, ae_object_t * const that);
ae_object_t *      ae_object_clone(ae_object_t * const this);
// ugly putses:
void               ae_object_fputs(const ae_object_t * const this, FILE * stream);
void               ae_object_puts(const ae_object_t * const this);
// pretty putses:
void               ae_object_fputsc(const ae_object_t * const this, FILE * stream);
void               ae_object_putsc(const ae_object_t * const this);
