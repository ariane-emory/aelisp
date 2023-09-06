#pragma once

#define MYLANG_OBJECT_HAS_UNION_MEMBER

#ifdef    USE_MYLANG_OBJECT
#  define YYSTYPE mylang_object_t
#else  // USE_MYLANG_OBJECT
#  define YYSTYPE mylang_string_t
#endif // USE_MYLANG_OBJECT

typedef char * mylang_string_t;

////////////////////////////////////////////////////////////////////////////////
// forward declaration
////////////////////////////////////////////////////////////////////////////////

typedef struct mylang_object_t mylang_object_t;
typedef struct mylang_list_t   mylang_list_t;

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
  DO(ML_PAREN)

#define enum_item(x) x,

typedef enum {
  ML_INVALID = 0,
  FOR_LEXED_TYPES_DO(enum_item)
} mylang_type_t;

const char * mylang_type_name(const mylang_type_t mylang_type);

////////////////////////////////////////////////////////////////////////////////
// Rational type
////////////////////////////////////////////////////////////////////////////////

typedef struct mylang_rational_t {
  int                    numerator;
  unsigned int           denominator;
} mylang_rational_t;

////////////////////////////////////////////////////////////////////////////////
// Data type
////////////////////////////////////////////////////////////////////////////////

typedef union {
  char *                 string_value;
  char                   char_value;
  int                    int_value;
  double                 float_value;
  mylang_rational_t      rational_value;
  mylang_list_t *        list_value;
} mylang_data_t;

////////////////////////////////////////////////////////////////////////////////
// Object type
////////////////////////////////////////////////////////////////////////////////

typedef struct mylang_object_t {
  mylang_type_t          type;
  mylang_string_t        c_str;
  mylang_data_t          data;
} mylang_object_t;

void mylang_object_init(mylang_object_t * const mylang_object);
void mylang_object_print(const mylang_object_t * const mylang_object);

////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

typedef struct mylang_list_t {
  mylang_object_t *      head;
  struct mylang_list_t * tail;
} mylang_list_t;
