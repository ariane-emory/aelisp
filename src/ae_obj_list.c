#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _length method
////////////////////////////////////////////////////////////////////////////////////////////////////

size_t ae_list_length(const ae_obj_t * const list) {
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

ae_obj_t * ae_list_map(const ae_obj_t * const list, ae_list_map_fun fun) {
  if (! list)
    return NULL;
  
  ASSERT_CONSP(list);

// #define MAP_RECURSES

#ifdef MAP_RECURSES
  if (! CAR(list))
    return NEW(AE_CONS____);

  return CONS(fun(CAR(list)), MAP(CDR(list), fun));
#else
  ae_obj_t * new_list = NEW(AE_CONS____);
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
  ASSERT_CONSP(list);

  ae_obj_t * new_list = NULL;
  
  FOR_EACH(elem, list) {
    if (EQ(elem, member))
      continue;
    else if (! new_list)
      new_list = CONS(elem, NULL);
    else 
      PUSH(new_list, elem);
  }
  
  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _cons
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_obj_cons(ae_obj_t * const head, ae_obj_t * const tail) {
  ASSERT_NEQ(head, tail); // consing an obj onto itself is not yet supported.
  
  if (tail)
    ASSERT_CONSP(tail);

#ifdef NOISY_INIT
  printf("Cons %p %p\n", head, tail);
  fflush(stdout);
#endif
  
  ae_obj_t * new_list = NEW(AE_CONS____);

  CAR(new_list) = head;
  CDR(new_list) = tail;

  return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _push_back
////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef NOISY_INIT
#  define AFTER_PUSH_MESSAGE(tailtip)                                                                                                       \
    fputs("Pushed           ", stdout);                                                                                                     \
    ae_obj_put(obj);                                                                                                                        \
    fputs(" into ", stdout);                                                                                                                \
    ae_obj_put(this);                                                                                                                       \
    fputs("' tailtip ", stdout);                                                                                                            \
    ae_obj_put(tailtip);                                                                                                                    \
    putchar('\n');                                                                                                                          \
    putchar('\n');
#else
#  define AFTER_PUSH_MESSAGE(tailtip) ((void)NULL)
#endif

ae_obj_t * ae_list_push_back(ae_obj_t * const list, ae_obj_t * const member) {
  ASSERT_CONSP(list);
 
#ifdef NOISY_INIT
  fputs("Pushing          ", stdout);
  ae_obj_put(member);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
#endif
  
  if (CAR(list)) {
    ae_obj_t * new_list = list;
    
    for (; CDR(new_list); new_list = CDR(new_list));

    CDR(new_list)       = CONS(member, NULL);

    AFTER_PUSH_MESSAGE(CDR(new_list));
  
    return CDR(new_list);
  }
  else {
    CAR(list) = member;

    AFTER_PUSH_MESSAGE(list);

    return list;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// intern
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NEW_SYM(sym) ae_obj_t * sym = NEW(AE_SYMBOL__); SYM_VAL(sym) = strdup(string)

ae_obj_t * ae_list_intern_string(ae_obj_t ** const plist, ae_string_t string) {
  if (! *plist)
    *plist = NEW(AE_CONS____);
  
  if (! CAR(*plist)) {
    // shortcut/hack for my weird imaginary nil:
    NEW_SYM(sym);
    
    return (CAR(*plist) = sym);
  }

  FOR_EACH(elem, *plist) {  
    if (strcmp(string, elem->sym_val) == 0) 
      return elem;
  }
     
  NEW_SYM(sym);
   
  return CAR(*plist = CONS(sym, *plist));
}

#undef NEW_SYM

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
