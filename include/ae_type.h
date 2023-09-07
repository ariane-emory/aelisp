#pragma once

////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////

#define FOR_LEXED_TYPES_DO(DO)                                                                                                              \
  DO(ML_STRING)                                                                                                                             \
  DO(ML_CHAR)                                                                                                                               \
  DO(ML_INTEGER)                                                                                                                            \
  DO(ML_FLOAT)                                                                                                                              \
  DO(ML_RATIONAL)                                                                                                                           \
  DO(ML_LIST)                                                                                                                               \
  DO(ML_SYMBOL)                                                                                                                             \
  DO(ML_QUOTE)                                                                                                                              \
  DO(ML_PAREN)

#define enum_node(x) x,

typedef enum {
  ML_INVALID = 0,
  FOR_LEXED_TYPES_DO(enum_node)
} ae_type_t;

const char * const ae_type_str(const ae_type_t ae_type);
