#pragma once

#include "ae_rational.h"
#include "ae_list.h"

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////
// Data type
////////////////////////////////////////////////////////////////////////////////

typedef union {
  ae_string_t               string_value;
  char                      char_value;
  int                       int_value;
  double                    float_value;
  ae_rational_t             rational_value;
  ae_list_t                 list_value;
} ae_data_t;

