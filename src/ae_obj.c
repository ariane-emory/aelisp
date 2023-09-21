#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t   true_obj     = { .metadata = AE_SYMBOL, .sym_val = "t"   };
ae_obj_t   nil_obj      = { .metadata = AE_SYMBOL, .sym_val = "nil" };
ae_obj_t * symbols_list = NULL;

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_type_str function
////////////////////////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_EACH_LEXED_TYPE(return_str);
    return_str(AE_FREE);
  default:
    return "UNRECOGNIZED";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////////////////////////
// _equal method
////////////////////////////////////////////////////////////////////////////////////////////////////

static int gcd(const int left, const int right) {
  int gcd = 0;
  
  for(int i = 1; i <= left && i <= right; ++i)
    if(left % i==0 && right % i==0)
      gcd = i;
    
  return gcd;
}

void ae_rational_simplify(ae_obj_t * this) {
  ASSERT_RATIONALP(this);
  
  int this_gcd = gcd(NUMER_VAL(this), DENOM_VAL(this));

  NUMER_VAL((ae_obj_t *)this) /= this_gcd;
  DENOM_VAL((ae_obj_t *)this) /= this_gcd;
}

bool ae_obj_equal (const ae_obj_t * const this,  const ae_obj_t *  const that) {
  if (this             == that)
    return true;
  
  if (CONSP    (this)  && CONSP    (that) &&
      CAR      (this)  == CAR      (that) &&
      CDR      (this)  == CDR      (that))
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

  if (RATIONALP (this)  && RATIONALP(that)) {
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
  /* Using GCC 13, it appears that the two possible return values' addresses are 1. distinct for
     either possible argument and 2. consistent for each possible argument when this function is
     called repeatedly, but I'm not sure if the standard gives any assurance that they always will
     be. Possibly UB?

     Probably a good idea to come up with a less sketchy way of implementing this...
  */
  
  return this ? TRU : NIL;

}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_init(ae_obj_t * const this, ae_type_t type) {

#ifdef AE_LOG_INIT
  fputs("Initializing     ", stdout);
  PUT(this);
  putchar('\n');
#endif

  ZERO(this);
  SET_TYPE(this, type);

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
  ASSERT_NEQ(this, that);
  
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
#ifdef AE_LOG_CLONE
  fputs("Cloning          ", stdout);
  PUT(this);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * clone = NULL;

#define CLONE_USING_MEMCPY clone = ALLOC(); memcpy(clone, this, sizeof(ae_obj_t))
#define DUP_C_STR(field) clone->field = strdup(this->field)
  
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

#define AE_TYPE_SHIFT 0
#define AE_FOOO_SHIFT AE_TYPE_BITS

#define AE_TYPE_MASK  (MASK(AE_TYPE_BITS, AE_TYPE_SHIFT))
#define AE_FOOO_MASK  (MASK(AE_FOOO_BITS, AE_FOOO_SHIFT))

#define MASK(size, shift)                   ((unsigned int) (((1 << (size)) - 1) << (shift)))
#define GET_MASKED(type, from, mask, shift) ((type) (((from) & (mask)) >> (shift)))
#define TO_MASKED(value, mask, shift)       (this->metadata & ~(mask)) | ((unsigned int)(value) << (shift))

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_type method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_type_t ae_obj_get_type(const ae_obj_t * const this) {
  ae_type_t type = GET_MASKED(ae_type_t, this->metadata, AE_TYPE_MASK, AE_TYPE_SHIFT);

#ifdef AE_LOG_METADATA
  printf("While getting type, metadata was 0x%016X, type is %d.\n", this->metadata, type);
#endif
  
  // This assertion should pass so long as ae_obj_set_foo hasn't been called yet.
  assert(this->metadata == type);

  return type;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_type method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_obj_set_type(ae_obj_t * const this, const ae_type_t type) {
  ae_type_t old_type = GET_MASKED(ae_type_t, this->metadata, AE_TYPE_MASK, AE_TYPE_SHIFT);
  this->metadata     = TO_MASKED (type, AE_TYPE_MASK, AE_TYPE_SHIFT);

#ifdef AE_LOG_METADATA
  printf("While setting type to %d, type was %d. Metadata is now 0x%016X.\n", type, old_type, this->metadata); 
#endif

  // This assertion should pass so long as ae_obj_set_foo hasn't been called yet.
  assert(this->metadata == type);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_foo method
////////////////////////////////////////////////////////////////////////////////////////////////////

// This is not yet used and is just here as an example of how to get the next metadata region:

char ae_obj_get_foo(const ae_obj_t * const this) {
  char foo = GET_MASKED(char, this->metadata, AE_FOOO_MASK, AE_FOOO_SHIFT);

#ifdef AE_LOG_METADATA
  printf("While getting foo, metadata was 0x%016X, foo is '%c' (%d).\n", this->metadata, foo, foo);
#endif

  return foo;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_foo method
////////////////////////////////////////////////////////////////////////////////////////////////////

// This is not yet used and is just here as an example of how to set the next metadata region:

void ae_obj_set_foo(ae_obj_t * const this, const char foo) {
  char old_foo   = GET_MASKED(char, this->metadata, AE_FOOO_MASK, AE_FOOO_SHIFT);
  this->metadata = TO_MASKED (foo, AE_FOOO_MASK, AE_FOOO_SHIFT);

#ifdef AE_LOG_METADATA 
  printf("While setting foo to '%c' (%d), foo was '%c' (%d). Metadata is now 0x%016X.\n", foo, foo, old_foo, old_foo, this->metadata);
#endif
}
