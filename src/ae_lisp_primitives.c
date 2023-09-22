#include "ae_lisp_primitives.h"
#include "ae_obj_list.h"

ae_obj_t *ae_lisp_car(ae_obj_t * const args) {
  ASSERT_TAILP(args);

  if (NILP(args))
    return NIL;
  
  ae_obj_t * first = CAR(args);
  
  if (NILP(first))
    return NIL;

  if (CONSP(first))
    return CAR(first);

  assert(0);
}

ae_obj_t *ae_lisp_cdr(ae_obj_t * const args) {
  ASSERT_TAILP(args);
  if (NILP(args))
    return NIL;

  ae_obj_t * first = CAR(args);

  if (NILP(first))
    return NIL;

  if (CONSP(first))
    return CDR(first);
  
  assert(0);
}

ae_obj_t *ae_lisp_cons(ae_obj_t * const args) {
  ASSERT_TAILP(args);
  if (NILP(args))
    return NIL;

  // return CONS(first, second);
  assert(0);
}
