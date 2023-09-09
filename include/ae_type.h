#pragma once

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
