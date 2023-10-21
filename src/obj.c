#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
 
#include "obj.h"
#include "list.h"
#include "pool.h"
#include "free_list.h"
#include "util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t   true_obj     = { .metadata = AE_SYMBOL, .sym_val = "t"   };
ae_obj_t   nil_obj      = { .metadata = AE_SYMBOL, .sym_val = "nil" };
ae_obj_t * symbols_list = NIL;

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_type_str function
////////////////////////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_EACH_LEXED_TYPE(return_str);
    return_str(AE_FREE);
  default:
    return "AE_UNRECOGNIZED";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql method
////////////////////////////////////////////////////////////////////////////////////////////////////

static int gcd(const int left, const int right) {
  int gcd = 0;
  
  for(int i = 1; i <= left && i <= right; ++i)
    if(left % i==0 && right % i==0)
      gcd = i;
    
  return gcd;
}

void ae_rational_simplify(ae_obj_t * const this) {
  assert(this);
  assert(RATIONALP(this));
  
  int this_gcd = gcd(NUMER_VAL(this), DENOM_VAL(this));

  NUMER_VAL((ae_obj_t *)this) /= this_gcd;
  DENOM_VAL((ae_obj_t *)this) /= this_gcd;
}

bool ae_obj_eql (const ae_obj_t * const this,  const ae_obj_t *  const that) {
  assert(this);
  assert(that);
      
  if (this             == that)
    return true;
  
  if (CONSP    (this)  && CONSP    (that) &&
      CAR      (this)  == CAR      (that) &&
      CDR      (this)  == CDR      (that))
    return true;
  
  if (STRINGP  (this)  && STRINGP  (that) &&
      ((STR_VAL(this)  == STR_VAL  (that)) ||
       (! strcmp(STR_VAL(this), STR_VAL(that)))))
    return true;

  if (CHARP    (this)  && CHARP    (that) &&
      CHAR_VAL (this)  == CHAR_VAL  (that))
    return true;
  
  if (INTEGERP (this)  && INTEGERP (that) &&
      INT_VAL  (this)  == INT_VAL  (that))
    return true;
  
  if (FLOATP   (this)  && FLOATP (that) &&
      FLOAT_VAL(this)  == FLOAT_VAL(that))
    return true;
  
  if (INTEGERP (this)  && FLOATP   (that) &&
      INT_VAL  (this)  == FLOAT_VAL(that))
    return true;

  if (FLOATP   (this)  && INTEGERP (that) &&
      FLOAT_VAL(this)  == INT_VAL  (that))
    return true;

  if (RATIONALP (this) && RATIONALP(that)) {
    // If they're both rationals we're going to cast away const and simplify
    // them both first before comparing their numerators and denominators.

    ae_rational_simplify((ae_obj_t *)this);
    ae_rational_simplify((ae_obj_t *)that);

    return (NUMER_VAL(this) == NUMER_VAL(that) &&
            DENOM_VAL(this) == DENOM_VAL(that));
  }

  if (RATIONALP (this)  && INTEGERP (that) &&
      ((int)(NUMER_VAL(this) / DENOM_VAL(this))) == INT_VAL(that))
    return true;

  if (INTEGERP (this)   && RATIONALP(that) &&
      INT_VAL(this)     == ((int)(NUMER_VAL(that) / DENOM_VAL(that))))
    return true;

  if (RATIONALP (this)  && FLOATP (that) &&
      ((int)(NUMER_VAL(this) / DENOM_VAL(this))) == FLOAT_VAL(that))
    return true;

  if (FLOATP (this)     && RATIONALP(that) &&
      FLOAT_VAL(this)   == ((int)(NUMER_VAL(that) / DENOM_VAL(that))))
    return true;

  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _truth method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_truth (const bool this) {
  return this ? TRUE : NIL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_init(ae_obj_t * const this, ae_type_t type) {
  assert(this);
  
#ifdef AE_LOG_INIT
  fputs("Initializing     ", stdout);
  PUT(this);
  putchar('\n');
#endif

  ZERO(this);
  SET_TYPE(this, type);

#ifdef AE_DEBUG_OBJ
  DOBJ(this) = NIL;
#endif
  
#ifdef AE_LOG_INIT
  fputs("Initialized      ", stdout);
  PUT(this);
  putchar('\n');
#endif

  return this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _unsafe_move method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_unsafe_move(ae_obj_t * const this, ae_obj_t * const that) {
  assert(this);
  assert(that);
  assert(this != that);
  
#ifdef AE_LOG_MOVE
  fputs("Moving           ", stdout);
  PUT(that);
  fputs(" to ", stdout);
  PUT(this);
  putchar('\n');
#endif

  COPY(this, that);
  INIT(that, AE_FREE);

#ifdef AE_LOG_MOVE
  fputs("Moved            ", stdout);
  PUT(that);
  fputs(" to ", stdout);
  PUT(this);
  putchar('\n');
#endif

  return this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _clone method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_clone(ae_obj_t * const this) {
  assert(this);
  
#ifdef AE_LOG_CLONE
  fputs("Cloning          ", stdout);
  PUT(this);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * clone = NULL;

#define CLONE_USING_MEMCPY clone = ALLOC(); COPY(clone, this)

#ifdef NO_AE_FREE_LIST
#  define DUP_C_STR(field) clone->field = strdup(this->field)
#else
#  define DUP_C_STR(field)                                                                         \
  {                                                                                                \
    clone->field = free_list_malloc(strlen(this->field) + 1);                                      \
    strcpy(clone->field, this->field);                                                             \
  }
#endif
  
  switch (GET_TYPE(this)) {
  case AE_CONS:
    clone = MAP(this, ae_obj_clone);
    break;
  case AE_STRING:
    CLONE_USING_MEMCPY;
    DUP_C_STR(str_val);
    break;
  case AE_SYMBOL:
    CLONE_USING_MEMCPY;
    DUP_C_STR(sym_val);
    break;
  default:
    CLONE_USING_MEMCPY;
  }

#undef CLONE_USING_MEMCPY
#undef DUP_C_STR
  
#ifdef AE_LOG_CLONE
  fputs("Cloned           ", stdout);
  PUT(this);
  fputs(" into ", stdout);
  PUT(clone);
  putchar('\n');
  fflush(stdout);
#endif

  return clone;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_obj_tailp(const ae_obj_t * const this) {
  assert(this);

  if (NIL == this)
    return true;

  if (ATOMP(this))
    return false;

  assert(CAR(this));

  return true;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _specialp
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_obj_specialp(const ae_obj_t * const this) {
  assert(this);

  return MACROP(this) || (COREP(this) && this->special);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _keywordp
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_obj_keywordp(const ae_obj_t * const this) {
  assert(this);

  return SYMBOLP(this) && SYM_VAL(this)[0] == ':';
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Metadata and its sub-fields
////////////////////////////////////////////////////////////////////////////////////////////////////
/*
  63   63 62    22 21      18 17      14 13    6 5     0  <-- Bit index
  +-------+--------+---------------------+-------+-------+
  | DELOC |  ...   | MAX_ARGS | MIN_ARGS | FOO   | TYPE  |
  +-------+--------+---------------------+-------+-------+
  |<--1-->|<--41-->|<----4--->|<----4--->|<--8-->|<--6-->|
*/
////////////////////////////////////////////////////////////////////////////////////////////////////

#define METADATA_TYPE          typeof(((ae_obj_t *)NULL)->metadata)
#define METADATA_SIZE          sizeof(((ae_obj_t *)NULL)->metadata)
#define METADATA_CAST(x)       ((METADATA_TYPE)(x))

// Mask, shifting, and extraction helpers
#define MASK(size, shift)                    (METADATA_CAST((METADATA_CAST(1) << (size)) - METADATA_CAST(1)) << (shift))
#define FROM_MASKED(type, from, mask, shift) (METADATA_CAST(((from) & (mask)) >> (shift)))
#define TO_MASKED(value, mask, shift)        (this->metadata & ~(mask)) | (METADATA_CAST(METADATA_CAST(value) << (shift)))

// Sub-field sizes
#define AE_TYPE_BITS           6
#define AE_FOO_BITS            8
#define AE_DELOC_BITS          1
#define AE_CORE_MIN_ARGS_BITS  4
#define AE_CORE_MAX_ARGS_BITS  4

// Sub-field shifts
#define AE_TYPE_SHIFT          0  // far right
#define AE_FOO_SHIFT           AE_TYPE_BITS
#define AE_CORE_MIN_ARGS_SHIFT (AE_FOO_SHIFT + AE_FOO_BITS)
#define AE_CORE_MAX_ARGS_SHIFT (AE_CORE_MIN_ARGS_SHIFT + AE_CORE_MIN_ARGS_BITS)
#define AE_DELOC_SHIFT         (METADATA_SIZE * 8 - AE_DELOC_BITS)  // highest bit

// Sub-field masks
#define AE_TYPE_MASK           (MASK(AE_TYPE_BITS,   AE_TYPE_SHIFT))
#define AE_FOO_MASK            (MASK(AE_FOO_BITS,    AE_FOO_SHIFT))
#define AE_CORE_MIN_ARGS_MASK  (MASK(AE_CORE_MIN_ARGS_BITS, AE_CORE_MIN_ARGS_SHIFT))
#define AE_CORE_MAX_ARGS_MASK  (MASK(AE_CORE_MAX_ARGS_BITS, AE_CORE_MAX_ARGS_SHIFT))
#define AE_DELOC_MASK          (MASK(AE_DELOC_BITS,  AE_DELOC_SHIFT))

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_type method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_type_t ae_obj_get_type(const ae_obj_t * const this) {
  assert(this);

  if (! this)
    return AE_INVALID;
  
  ae_type_t type = FROM_MASKED(ae_type_t, this->metadata, AE_TYPE_MASK, AE_TYPE_SHIFT);

  return type;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_type method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_type(ae_obj_t * const this, const ae_type_t type) {
  assert(this);
  
  ae_type_t old_type = FROM_MASKED(ae_type_t, this->metadata, AE_TYPE_MASK, AE_TYPE_SHIFT);
  this->metadata     = TO_MASKED (type, AE_TYPE_MASK, AE_TYPE_SHIFT);

  assert(this->metadata == type);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_foo method
////////////////////////////////////////////////////////////////////////////////////////////////////

// This is not yet used and is just here as an example of how to get the next metadata region:

char ae_obj_get_foo(const ae_obj_t * const this) {
  assert(this);
  
  char foo = FROM_MASKED(char, this->metadata, AE_FOO_MASK, AE_FOO_SHIFT);

#ifdef AE_LOG_METADATA
  //PR("While getting foo, metadata was 0x%08X, foo is '%c' (%d).\n", this->metadata, foo, foo);
#endif

  return foo;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_foo method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_foo(ae_obj_t * const this, const char foo) {
  assert(this);
  
  char old_foo   = FROM_MASKED(char, this->metadata, AE_FOO_MASK, AE_FOO_SHIFT);
  this->metadata = TO_MASKED (      foo,            AE_FOO_MASK, AE_FOO_SHIFT);

#ifdef AE_LOG_METADATA 
  PR("While setting foo to '0x%016X' (%d), foo was '%c' (%d). Metadata is now 0x%08X.\n",
     foo, foo, old_foo, old_foo, this->metadata);
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_deloc method
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_obj_get_delocalized(const ae_obj_t * const this) {
  assert(this);
  
  bool deloc = FROM_MASKED(bool, this->metadata, AE_DELOC_MASK, AE_DELOC_SHIFT) ? true : false;

#ifdef AE_LOG_METADATA
  PR("While getting deloc of %016p, metadata was 0x%08X, deloc is %d.\n", this->metadata, deloc);
#endif

  return deloc;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_deloc method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_delocalized(ae_obj_t * const this, const bool deloc) {
  assert(this);

#ifdef AE_LOG_METADATA
  printf("Before: 0x%08X\n", this->metadata);
  
  printf("mask is  %008X, shift is %008X.\n", AE_DELOC_MASK, AE_DELOC_SHIFT);
#endif
  
  bool old_deloc   = FROM_MASKED(bool, this->metadata, AE_DELOC_MASK, AE_DELOC_SHIFT);
  this->metadata   = TO_MASKED (      deloc ? 1 : 0,  AE_DELOC_MASK, AE_DELOC_SHIFT);
  
#ifdef AE_LOG_METADATA
  printf("While setting deloc of %016p to (0x%08X), deloc was 0x%08X. Metadata is now 0x%08X.\n",
         deloc, deloc, old_deloc, this->metadata);
  printf("After:  0x%08X\n", this->metadata);
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_max_args method
////////////////////////////////////////////////////////////////////////////////////////////////////

unsigned int ae_obj_get_max_args(const ae_obj_t * const this) {
  assert(this);
    
  unsigned int max_args = FROM_MASKED(unsigned int, this->metadata,
                                      AE_CORE_MAX_ARGS_MASK, AE_CORE_MAX_ARGS_SHIFT);
  return max_args;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_max_args method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_max_args(ae_obj_t * const this, const unsigned int max_args) {
  assert(this);

  this->metadata = TO_MASKED(max_args, AE_CORE_MAX_ARGS_MASK, AE_CORE_MAX_ARGS_SHIFT);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_min_args method
////////////////////////////////////////////////////////////////////////////////////////////////////

unsigned int ae_obj_get_min_args(const ae_obj_t * const this) {
  assert(this);
    
  unsigned int min_args = FROM_MASKED(unsigned int, this->metadata,
                                      AE_CORE_MIN_ARGS_MASK, AE_CORE_MIN_ARGS_SHIFT);
  return min_args;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_min_args method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_min_args(ae_obj_t * const this, const unsigned int min_args) {
  assert(this);
    
  this->metadata = TO_MASKED(min_args, AE_CORE_MIN_ARGS_MASK, AE_CORE_MIN_ARGS_SHIFT);
}

