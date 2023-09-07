#pragma once

#define YYSTYPE ae_object_t

typedef char * ae_string_t;

////////////////////////////////////////////////////////////////////////////////
// Forward declaration
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_object_t ae_object_t;
typedef struct ae_list_item_t   ae_list_item_t;

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

#define enum_item(x) x,

typedef enum {
  ML_INVALID = 0,
  FOR_LEXED_TYPES_DO(enum_item)
} ae_type_t;

// const char * const ae_type_str(const ae_type_t ae_type);

////////////////////////////////////////////////////////////////////////////////
// Rational type
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_rational_t {
  int                           numerator;
  unsigned int                  denominator;
} ae_rational_t;

////////////////////////////////////////////////////////////////////////////////
// Data type
////////////////////////////////////////////////////////////////////////////////

typedef union {
  char *                        string_value;
  char                          char_value;
  int                           int_value;
  double                        float_value;
  ae_rational_t             rational_value;
  ae_list_item_t *          list_value;
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

void ae_object_init(ae_object_t * const ae_object);
const char * const ae_object_str(const ae_object_t * const ae_object);

////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_list_item_t {
  ae_object_t *             object;
  struct ae_list_item_t *   tail;
} ae_list_item_t;

typedef ae_list_item_t * ae_list_t;

//------------------------------------------------------------------------------

void ae_list_init(ae_list_t * const ae_list);
const char * const ae_list_str(const ae_list_t * const ae_list);
void ae_list_append(ae_list_t * const ae_list, ae_object_t * ae_object);

void ae_list_item_init(ae_list_item_t * const ae_list_item);
const char * const ae_list_item_str(const ae_list_item_t * const ae_list_item);
void ae_list_item_append(ae_list_item_t * const ae_list_item, ae_object_t * ae_object);
