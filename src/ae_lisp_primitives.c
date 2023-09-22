#include "ae_lisp_primitives.h"
#include "ae_obj_list.h"

#define DIE assert(0)

ae_obj_t *ae_lisp_car(ae_obj_t * const args) {
  ASSERT_NOT_NILP(args);
  ASSERT_CONSP(args);
  
  ae_obj_t * first = CAR(args); // should be a list or nil
  ASSERT_TAILP(first);
  
  if (NILP(first))
    return NIL;

  return CAR(first);
}

ae_obj_t *ae_lisp_cdr(ae_obj_t * const args) {
  ASSERT_NOT_NILP(args);
  ASSERT_CONSP(args);
  
  ae_obj_t * first = CAR(args); // should be a list or nil
  ASSERT_TAILP(first);
  
  if (NILP(first))
    return NIL;

  return CAR(first);
}

ae_obj_t *ae_lisp_cons(ae_obj_t * const args) {
  ASSERT_TAILP(args);
  if (NILP(args))
    return NIL;

  // return CONS(first, second);
  assert(0);
}
