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

// This only deals with AE_INTEGERS for now. It mutates it's first argument.
#define DEF_MATH_OP(name, oper, default)                                                           \
ae_obj_t * ae_lisp_##name(ae_obj_t * const args) {                                                 \
  ASSERT_CONSP(args);                                                                              \
                                                                                                   \
  ae_obj_t * accum = NIL;                                                                          \
  ae_obj_t * rest  = NIL;                                                                          \
                                                                                                   \
  if (NILP(CDR(args))) {                                                                           \
    accum = NEW_INT(default);                                                                      \
    rest = args;                                                                                   \
  }                                                                                                \
  else {                                                                                           \
    ASSERT_INTEGERP(CAR(args));                                                                    \
    accum = CAR(args);                                                                             \
    rest = CDR(args);                                                                              \
  }                                                                                                \
                                                                                                   \
  FOR_EACH(elem, rest) {                                                                           \
    ASSERT_INTEGERP(elem);                                                                         \
    INT_VAL(accum) = INT_VAL(accum) oper INT_VAL(elem);                                            \
  }                                                                                                \
                                                                                                   \
  return accum;                                                                                    \
}

FOR_EACH_MATH_OP(DEF_MATH_OP);

  
