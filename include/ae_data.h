#pragma once

#include "ae_rational.h"
#include "ae_type.h"

#define YYSTYPE ae_object_t

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////
// Forward declaration
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_object_t ae_object_t;
typedef struct ae_list_node_t   ae_list_node_t;

////////////////////////////////////////////////////////////////////////////////
// Data type
////////////////////////////////////////////////////////////////////////////////

typedef union {
  char *                    string_value;
  char                      char_value;
  int                       int_value;
  double                    float_value;
  ae_rational_t             rational_value;
  ae_list_node_t *          list_value;
} ae_data_t;

////////////////////////////////////////////////////////////////////////////////
// Object type
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_object_t {
  ae_type_t                 type;
  ae_string_t               c_str;
  ae_data_t                 data;
} ae_object_t;

//------------------------------------------------------------------------------

void               ae_object_init(ae_object_t * const ae_object);
const char * const ae_object_str(const ae_object_t * const ae_object);
