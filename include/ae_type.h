#pragma once

////////////////////////////////////////////////////////////////////////////////
// Types enum
////////////////////////////////////////////////////////////////////////////////

#define FOR_LEXED_TYPES_DO(DO)                                                                                                              \
  DO(AE_STRING)                                                                                                                             \
  DO(AE_CHAR)                                                                                                                               \
  DO(AE_INTEGER)                                                                                                                            \
  DO(AE_FLOAT)                                                                                                                              \
  DO(AE_RATIONAL)                                                                                                                           \
  DO(AE_LIST)                                                                                                                               \
  DO(AE_SYMBOL)                                                                                                                             \
  DO(AE_QUOTE)                                                                                                                              \
  DO(AE_PAREN)

#define enum_node(x) x,

typedef enum {
  AE_INVALID = 0,
  FOR_LEXED_TYPES_DO(enum_node)
} ae_type_t;

const char * const ae_type_str(const ae_type_t this);
