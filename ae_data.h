#pragma once

#include "ae_rational.h"
#include "ae_list.h"

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

