#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_type_str method
////////////////////////////////////////////////////////////////////////////////////////////////////

#define return_str(x) case x: return #x;
const char * ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_EACH_LEXED_TYPE(return_str);
    return_str(AE_FREE____);
  default:
    return "UNRECOGNIZED";
  }
}
#undef return_str

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
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
  /* todo: handle rationals */

  fputs("\nThis ", stdout);
  fputs(ae_type_str(this->type), stdout);
  putchar('\n');

  fputs("That ", stdout);
  fputs(ae_type_str(that->type), stdout);
  putchar('\n');

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

    printf("here\n");
    fputs("this ", stdout);
    WRITE(this);
    putchar('\n');
    printf("here\n");

    fputs("that ", stdout);
    WRITE(that);
    putchar('\n');
    
    bool x =  (NUMER_VAL(this) == NUMER_VAL(that) &&
               DENOM_VAL(this) == DENOM_VAL(that));

    puts(x ? "yes" : "no");

    return x;
  }

  if (RATIONALP (this)  && INTEGERP (that) &&
      ((int)(NUMER_VAL(this) / DENOM_VAL(this))) == INT_VAL(that))
    return true;

  if (INTEGERP (this)   && RATIONALP(that) &&
      INT_VAL(this)     == ((int)(NUMER_VAL(this) / DENOM_VAL(that))))
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
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_truth (const bool this) {
  /* Using GCC 13, it appears that the two possible return values' addresses are 1. distinct for
     either possible argument and 2. consistent for each possible argument when this function is
     called repeatedly, but I'm not sure if the standard gives any assurance that they always will
     be. Possibly UB?

     Probably a good idea to come up with a less sketchy way of implementing this...
  */
  
  return this
    ? &((ae_obj_t){ .type = AE_INTEGER, .int_val = 1               })
    : &((ae_obj_t){ .type = AE_CONS, .head = NULL, .tail = NULL });
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_init(ae_obj_t * const this, ae_type_t type) {

#ifdef NOISY_INIT
  fputs("Initializing     ", stdout);
  PUT(this);
  putchar('\n');
#endif

  ZERO(this);
  TYPE(this) = type;

#ifdef NOISY_INIT
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
  
#ifdef NOISY_INIT
  fputs("Moving           ", stdout);
  PUT(that);
  fputs(" to ", stdout);
  PUT(this);
  putchar('\n');
#endif

  COPY(this, that);
  INIT(that, AE_FREE____);

#ifdef NOISY_INIT
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

ae_obj_t * ae_obj_clone(const ae_obj_t * const this) {
#ifdef NOISY_INIT
  fputs("Cloning          ", stdout);
  PUT(this);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * clone = NULL;

#define CLONE_USING_MEMCPY clone = ALLOC(); memcpy(clone, this, sizeof(ae_obj_t))
#define DUP_C_STR(field) clone->field = strdup(this->field)
  
  switch (TYPE(this)) {
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
  
#ifdef NOISY_INIT
  fputs("Cloned           ", stdout);
  PUT(this);
  fputs(" into ", stdout);
  PUT(clone);
  putchar('\n');
  fflush(stdout);
#endif

  return clone;
}
