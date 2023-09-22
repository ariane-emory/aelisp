#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"

#define TAILP(o)        (NOT_NULLP((o)) && (NILP((o)) || (CONSP((o)) && CAR((o)))))
#define ASSERT_TAILP(o) (assert(TAILP((o))))

////////////////////////////////////////////////////////////////////////////////////////////////////
// _length method
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_list_length(const ae_obj_t * const list) {
  ASSERT_TAILP(list);
  
  if (NILP(list)) return 0;

  size_t length = 0;

  FOR_EACH_CONST(elem, list)
    length++;

  return length;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _each method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_list_each (ae_obj_t * const list, ae_list_each_fun fun) {
  ASSERT_TAILP(list);

  if (NILP(list))
    return;

#ifdef AE_LIST_EACH_RECURSES
  if (! CAR(list))
    return;

  fun(CAR(list));

  if (CDR(list))
    EACH(CDR(list), fun);
#else
  FOR_EACH(elem, list)
    fun(elem);
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _map method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_list_map(ae_obj_t * const list, ae_list_map_fun fun) {
  ASSERT_TAILP(list);

  if (NILP(list))
    return list;

#ifdef AE_LIST_MAP_RECURSES
  return CONS(fun(CAR(list)), MAP(CDR(list), fun));
#else
  ae_obj_t * new_list = CONS_NEW(fun(CAR(list)));

  if (NILP(CDR(list)))
    return new_list;
  
  ae_obj_t * tailtip  = new_list;

  FOR_EACH_CONST(elem, CDR(list))
    tailtip = PUSH(&tailtip, fun(elem));

  return new_list;
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_list_has_member(const ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_TAILP(list);
  ASSERT_NOT_NULLP(member);
  
  if (NILP(list))
    return false;
  
  FOR_EACH_CONST(elem, list)
    if (EQ(elem, member))
      return true;
  
  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_list_remove_member(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_TAILP(list);
  ASSERT_NOT_NULLP(member);
  
  if (NILP(list))
    return list;
  
  ae_obj_t * new_list = NIL;
  
  FOR_EACH(elem, list)
    if (EQ(elem, member))
      continue;
    else if (NILP(new_list))
      new_list = CONS_NEW(elem);
    else
      // this could be faster if we stashed the tailtip.
      PUSH(&new_list, elem);
  
  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  ASSERT_TAILP(tail);
  ASSERT_NOT_NULLP(head);

#ifdef AE_LOG_CONS
  fputs("Cons ", stdout);
  PUT(head);
  fputs(" onto ", stdout);
  
  if (NILP(tail)) {
    fputs("nil", stdout);
  }
  else {
    PUT(tail);
    putchar(' ');
    WRITE(tail);
  }
  
  fputs("\n", stdout);
  fflush(stdout);
#endif
  
  ae_obj_t * new_list = NEW(AE_CONS);

  CAR(new_list) = head;
  CDR(new_list) = tail;

  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef AE_LOG_PUSH
#  define AFTER_PUSH_MESSAGE(tailtip)                                                              \
  fputs("Pushed           ", stdout);                                                              \
  PUT(member);                                                                                     \
  fputs(" into ", stdout);                                                                         \
  PUT(list);                                                                                       \
  fputs("'s new tailtip ", stdout);                                                                \
  PUT(tailtip);                                                                                    \
  putchar('\n');
#else
#  define AFTER_PUSH_MESSAGE(tailtip) ((void)NULL)
#endif

ae_obj_t * ae_list_push_back(ae_obj_t ** const plist, ae_obj_t * const member) {
  // This takes a ** because: if the caller tries to push onto NIL, it might create a new list and
  // fix their pointer.

  // Return value is the tailtip of the list, so repeated pushes can be performed more performantly
  // by pushing onto the return value of a prior push.
  
  ASSERT_TAILP(*plist);
  ASSERT_NOT_NULLP(member);

  if (NILP(*plist)) {
    ae_obj_t * new_list = CONS(member, *plist);
    *plist = new_list;
    return new_list;
  }
  
#ifdef AE_LOG_PUSH
  fputs("Pushing          ", stdout);
  PUT(member);
  fputs(" into ", stdout);
  PUT(*plist);
  putchar(' ');
  WRITE(*plist);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * tailtip = *plist;
    
  for (; NOT_NILP(CDR(tailtip)); tailtip = CDR(tailtip));

  CDR(tailtip) = CONS_NEW(member);

  AFTER_PUSH_MESSAGE(CDR(tailtip));
  
  return CDR(tailtip);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NEW_SYM ae_obj_t * sym = NEW(AE_SYMBOL); SYM_VAL(sym) = strdup(string)

ae_obj_t * ae_list_intern_string(ae_obj_t ** const plist, ae_string_t string) {
  assert(NULLP(*plist) || TAILP(*plist));
  ASSERT_NOT_NULLP(string);
  
#ifdef AE_LOG_INTERN2
  // pool_print();
  // putchar('\n');
  printf("Interning \"%s\" in %p.\n", string, *plist);
  fflush(stdout);
#endif

  if (! *plist) {
    NEW_SYM;
    *plist = CONS_NEW(sym);
    
#ifdef AE_LOG_INTERN2
    printf("Intern in new symbol list ");
    PUT(*plist);
    putchar('\n');

    printf("=> ");
    PUT(sym);
    putchar('\n');
#endif

    return sym;
  }

  FOR_EACH(elem, *plist) {
    ASSERT_SYMBOLP(elem);
    if (strcmp(string, elem->sym_val) == 0) {
#ifdef AE_LOG_INTERN2
      printf("Intern in symbol list ");
      PUT(*plist);
      putchar('\n');

      printf("=> reused ");
      PUT(elem);
      putchar('\n');
#endif

      return elem;
    }
  }
  
  NEW_SYM;
  *plist = CONS(sym, *plist);

#ifdef AE_LOG_INTERN2
  printf("Intern in symbol list ");
  PUT(*plist);
  putchar('\n');

  printf("=> ");
  PUT(sym);
  putchar('\n');
#endif

  return sym;
}

#undef DECLARE_NEW_SYM

