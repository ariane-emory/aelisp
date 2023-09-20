#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _length method
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_list_length(const ae_obj_t * const list) {
  ASSERT_CONSP(list);
  
  if (! CAR(list) ) return 0;

  size_t length = 0;

  FOR_EACH_CONST(elem, list)
    length++;

  return length;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _each method
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_list_each (ae_obj_t * const list, ae_list_each_fun fun) {
  ASSERT_CONSP(list);

// #define EACH_RECURSES

#ifdef EACH_RECURSES
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
#define MAP_RECURSES

#ifdef MAP_RECURSES
  if (NILP(list))
    return list;

  return CONS(fun(CAR(list)), MAP(CDR(list), fun));
  
#else
  ae_obj_t * new_list = NEW(AE_CONS);
  ae_obj_t * tailtip  = new_list;

  FOR_EACH_CONST(elem, list)
    tailtip = ae_list_push_back(tailtip, fun(elem));

  return new_list;
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_list_has_member(const ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);

  FOR_EACH_CONST(elem, list)
    if (EQ(elem, member))
      return true;
  
  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_member method
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_list_remove_member(ae_obj_t * const list, ae_obj_t * const member) {
  if (NILP(list))
    return list;
  
  ASSERT_CONSP(list);

  ae_obj_t * new_list = NIL;
  
  FOR_EACH(elem, list)
    if (EQ(elem, member))
      continue;
    else if (NILP(new_list))
      new_list = CONS(elem, NIL);
    else 
      PUSH(new_list, elem);
  
  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  assert(NOT_NULLP(tail) &&
         (NILP(tail) ||
          CONSP(tail)));
  
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

ae_obj_t * ae_list_push_back(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);
 
#ifdef AE_LOG_PUSH
  fputs("Pushing          ", stdout);
  PUT(member);
  fputs(" into ", stdout);
  PUT(list);
  putchar(' ');
  WRITE(list);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * preexisting_cons = list;
    
  for (;
       ! NILP(CDR(preexisting_cons));
       preexisting_cons = CDR(preexisting_cons));

  CDR(preexisting_cons)       = CONS(member, NIL);

  AFTER_PUSH_MESSAGE(CDR(preexisting_cons));
  
  return CDR(preexisting_cons);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NEW_SYM ae_obj_t * sym = NEW(AE_SYMBOL); SYM_VAL(sym) = strdup(string)

ae_obj_t * ae_list_intern_string(ae_obj_t ** const plist, ae_string_t string) {
  assert(string);
  
#ifdef AE_LOG_INTERN
  // pool_print();
  // putchar('\n');
  printf("Interning \"%s\" in %p.\n", string, *plist);
  fflush(stdout);
#endif

  if (! *plist) {
    NEW_SYM;
    *plist = CONS(sym, NIL);
    
#ifdef AE_LOG_INTERN
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
#ifdef AE_LOG_INTERN
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

#ifdef AE_LOG_INTERN
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

