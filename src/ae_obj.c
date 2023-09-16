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

bool ae_obj_equal (const ae_obj_t * const this,  const ae_obj_t *  const that) {
  if      (this            == that)
    return true;
  else if (CONSP   (this)  && CONSP    (that) &&
           CAR     (this)  == CAR      (that) &&
           CDR     (this)  == CDR      (that))
    return true;
  else if (INTEGERP(this)  && INTEGERP (that) &&
           INT_VAL (this)  == INT_VAL  (that))
    return true;
  else if (FLOATP   (that) && INTEGERP (this) &&
           FLOAT_VAL(that) == INT_VAL  (this))
    return true;

  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _init method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_truth (const bool this);

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
  case AE_CONS____:
    clone = MAP(this, ae_obj_clone);
    break;
  case AE_STRING__:
    CLONE_USING_MEMCPY;
    DUP_C_STR(str_val);
    break;
  case AE_SYMBOL__:
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
