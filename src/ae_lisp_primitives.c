#include "ae_lisp_primitives.h"
#include "ae_obj_list.h"

#define DIE     (assert(0))
#define NL      (putchar('\n'))
#define SPC     (putchar(' '))
#define PR(...) (fprintf(stdout, __VA_ARGS__))

////////////////////////////////////////////////////////////////////////////////////////////////////
// _car
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_car(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NILP(CDR(args));  // takes only one arg.
  ASSERT_TAILP(CAR(args)); // which must be a tail.

  if (NILP(CAR(args)))     // car of nil is nil.
    return NIL;
  
  return CAAR(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cdr
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_cdr(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NILP(CDR(args));  // takes only one arg.
  ASSERT_TAILP(CAR(args)); // which must be a tail.
  
  if (NILP(CAR(args)))     // cdr of nil is nil.
    return NIL;

  return CDAR(args); 
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_cons(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_TAILP(CDR(args)); // 2nd arg must be a tail.
  ASSERT_NILP(CDDR(args)); // only 2 args.

  ae_obj_t * head = CAR(args);
  ae_obj_t * tail = CADR(args);
  
  return CONS(head, tail);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_eq(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NOT_NILP(CDR(args));

  FOR_EACH(tailarg, CDR(args))
    if (NEQ(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_eql(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  ASSERT_NOT_NILP(CDR(args));

  FOR_EACH(tailarg, CDR(args))
    if (NEQL(CAR(args), tailarg))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _atomp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_atomp(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  FOR_EACH(elem, args)
    if (! ATOMP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_not(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  FOR_EACH(elem, args)
    if (NOT_NILP(elem))
      return NIL;

  return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_print(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  NL;
  
  int written = 1;
  
  FOR_EACH(elem, args) {
    written += WRITE(elem);

    if (NOT_NILP(CDR(position))) {
      SPC;
      written++;
    }
  }

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _princ
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_princ(ae_obj_t * const args) {
  ASSERT_CONSP(args);
  
  int written = 0;
  
  FOR_EACH(elem, args) {
    written += WRITE(elem);
  }

  return NEW_INT(written);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

// This only deals with AE_INTEGERS properly for now:
// #define DEF_MATH_OP(name, oper, default)                                                           \
// ae_obj_t * ae_lisp_##name(ae_obj_t * const args) { \
//   ASSERT_CONSP(args); \
//

ae_obj_t * ae_lisp_add(ae_obj_t * const args) {
  ASSERT_CONSP(args);

  ae_obj_t * head = NIL;
  ae_obj_t * tail = NIL;

  NL;
  PR("args "); WRITE(args); NL;
  
  if (NILP(CDR(args))) {
    head = NEW_INT(0);
    tail = args;
  }
  else {
    ASSERT_INTEGERP(CAR(args));

    head = CAR(args); PR("head "); PUT(head); NL;
    tail = CDR(args); PR("tail "); WRITE(tail); NL;
  }
  
  NL;
  PR("Start with "); PUT(head); NL;
  
  FOR_EACH(elem, tail) {
    ASSERT_INTEGERP(elem);
    PR("Oper with  "); PUT(elem); NL;
    INT_VAL(head) = INT_VAL(head) + INT_VAL(elem);
  }

  PR("Return     "); PUT(head); NL;

  return head;
}
  
