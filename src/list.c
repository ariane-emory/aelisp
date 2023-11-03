#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "list.h"

#include "log.h"
#include "free_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _length method
////////////////////////////////////////////////////////////////////////////////////////////////////
int ae_list_length(const ae_obj_t * const list) {
  assert(list);
  assert(TAILP(list));
  
  if (NILP(list))
    return 0;

  size_t length = 0;

  FOR_EACH_CONST(elem, list)
    length++;

  return length;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _each method
////////////////////////////////////////////////////////////////////////////////////////////////////
void ae_list_each (ae_obj_t * const list, ae_list_each_fun fun) {
  assert(list);
  assert(TAILP(list));
  assert(fun);

  if (NILP(list))
    return;

#ifdef AE_LIST_EACH_RECURSES
  if (!CAR(list))
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


////////////////////////////////////////////////////////////////////////////////////////////////////
// _map method
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_map(ae_obj_t * const list, ae_list_map_fun fun) {
  assert(list);
  assert(TAILP(list));
  assert(fun);
  
  if (!(PROPERP(list)))
    return NIL;

  if (NILP(list))
    return list;

#ifdef AE_LIST_MAP_RECURSES
  return CONS(fun(CAR(list)), MAP(CDR(list), fun));
#else
  ae_obj_t * new_list = NEW_CONS(fun(CAR(list)), NIL);
  ae_obj_t * tailtip  = new_list;

  if (NILP(CDR(list)))
    return new_list;

  FOR_EACH_CONST(elem, CDR(list))
    tailtip = PUSH_BACK(tailtip, fun(elem));

  return new_list;
#endif
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_member method
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_list_has_member(const ae_obj_t * const list, ae_obj_t * const member) {
  assert(list);
  assert(TAILP(list));
  assert(member);
  
  if (NILP(list))
    return false;
  
  FOR_EACH_CONST(elem, list)
    if (elem == member)
      return true;
  
  return false;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_member method
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_remove_member(ae_obj_t * const list, ae_obj_t * const member) {
  assert(list);
  assert(TAILP(list));
  assert(member);
  
  if (NILP(list))
    return list;
  
  // This is non-mutating: it 'removes' member by returning a new list that doesn't contain it.

  ae_obj_t * new_list = NIL;
  
  FOR_EACH(elem, list)
    if (elem == member)
      continue;
    else if (NILP(new_list))
      new_list = NEW_CONS(elem, NIL);
    else
      // this could be faster if we stashed the tailtip.
      PUSH_BACK(new_list, elem);
  
  return new_list;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  assert(head);
  assert(tail);
  
  if (!(TAILP(tail)))
    fprintf(stderr, "\nCan't cons onto a %s!\n", GET_TYPE_STR(tail));
  
  assert(TAILP(tail));

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
    PRINC(tail);
  }
  
  fputs("\n", stdout);
  fflush(stdout);
#endif
  
  ae_obj_t * new_list = NEW(AE_CONS);

  CAR(new_list)       = head;
  CDR(new_list)       = tail;

  return new_list;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_LOG_PUSH
#  define AFTER_PUSH_BACK_MESSAGE(tailtip)                                                         \
  fputs("Pushed           ", stdout);                                                              \
  PUT(member);                                                                                     \
  fputs(" into ", stdout);                                                                         \
  PUT(list);                                                                                       \
  fputs("'s new tailtip ", stdout);                                                                \
  PUT(tailtip);                                                                                    \
  putchar('\n');
#else
#  define AFTER_PUSH_BACK_MESSAGE(tailtip) ((void)NULL)
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_push_back(ae_obj_t ** const plist, ae_obj_t * const member) {
  // This takes a ** because: if the caller tries to push onto NIL, it might create a new list and
  // fix their pointer.

  // Return value is the tailtip of the list, so repeated pushes can be performed more performantly
  // by pushing onto the return value of a prior push.

  assert(plist);
  assert(*plist);
  assert(TAILP(*plist));
  assert(member);

  if (NILP(*plist))
    return *plist = CONS(member, *plist);
  
#ifdef AE_LOG_PUSH
  fputs("Pushing          ", stdout);
  PUT(member);
  fputs(" onto the back of ", stdout);
  PUT(*plist);
  putchar(' ');
  PRINC(*plist);
  putchar('\n');
  fflush(stdout);
#endif
  
  ae_obj_t * tailtip = *plist;
  
  FOR_EACH(ignored, *plist) 
    tailtip = position;

  CDR(tailtip) = NEW_CONS(member, NIL);

  AFTER_PUSH_BACK_MESSAGE(CDR(tailtip));
  
  return CDR(tailtip);
}
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_push(ae_obj_t ** const plist, ae_obj_t * const member) {
  // This takes a ** because it's going to mutate the targe ae_obj_t  *.

  assert(plist);
  assert(*plist);
  assert(TAILP(*plist));
  assert(member);

  *plist = CONS(member, *plist);

  return *plist;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_pop(ae_obj_t ** const plist) {
  // This takes a ** because it's going to mutate the targe ae_obj_t  *.

  assert(plist);
  assert(*plist);
  assert(CONSP(*plist)); // don't pop from an empty list!

  ae_obj_t * const head = CAR(*plist);

  *plist = CDR(*plist);

  return head;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////////////////////////
#define NEW_SYM                                                                                    \
  ae_obj_t * new_sym;                                                                              \
  {                                                                                                \
    char * _str = free_list_malloc(strlen(string) + 1);                                            \
    assert(((void)"Failed alloc!", _str));                                                         \
    assert(((void)"Null string!", string));                                                        \
    strcpy(_str, string);                                                                          \
    new_sym = NEW_SYMBOL(_str);                                                                    \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_intern_string(ae_obj_t ** const plist, const char * const string) {
  assert(plist);
  assert((! *plist) || TAILP(*plist));
  assert(string);
  assert(strlen(string) != 0);
  
  if (! strcmp(string, "nil"))
    return NIL;

  if (! strcmp(string, "t")  )
    return TRUE;
  
#ifdef AE_LOG_SYM
  putchar('\n');
  printf("Interning \"%s\" in %p.\n", string, *plist);
  fflush(stdout);
#endif

  if (NILP(*plist)) {
    NEW_SYM;
    *plist = NEW_CONS(new_sym, NIL);

    return new_sym;
  }
  
  FOR_EACH(elem, *plist) {
    if (! SYMBOLP(elem)) {
      fprintf(stderr, "\nCan't intern \"%s\" in a list containing a %s!\n",
              string, GET_TYPE_STR(elem));
      fprintf(stderr, "symbols list: ");
      FWRITE(*plist, stderr);
      fputc('\n', stderr);

      assert(0);
    }
    
    if (! strcmp(string, elem->sym_val)) {
#ifdef AE_LOG_SYM
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
  *plist = CONS(new_sym, *plist);

#ifdef AE_LOG_SYM
  printf("Intern in symbol list ");
  PUT(*plist);
  putchar('\n');

  printf("=> ");
  PUT(new_sym);
  putchar('\n');
#endif

  return new_sym;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _is_proper
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_list_is_proper(const ae_obj_t * const list) {
  assert(list);
  
  if (! TAILP(list))
    return false;
  
  FOR_EACH_CONST(elem, list)
    if (!(TAILP(CDR(position))))
      return false;

  return true;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _join3
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_list_join3(ae_obj_t * front, ae_obj_t * const middle, ae_obj_t * const  back) {
  if (NILP(front)) {
    front = middle;
  }
  else {
    ae_obj_t * pos = front;

    while (CONSP(CDR(pos)))
      pos = CDR(pos);

    CDR(pos) = middle;
  }
  
  if (NILP(front))
    return back;

  ae_obj_t * pos = front;
    
  while (CONSP(CDR(pos)))
    pos = CDR(pos);

  CDR(pos) = back;

  return front;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
