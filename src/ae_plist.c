#include "ae_list.h"
#include "ae_plist.h"

#define FOR_EACH_PAIR(elem1, elem2, list)                       \
  for (ae_obj_t * position = (list),                            \
                * elem1    = CAR(position),                     \
                * elem2    = position ? CADR(position) : NULL;  \
       elem1 && elem2;                                          \
       position = position ? CDR(CDR(position)) : NULL,         \
       elem1    = position ? CAR(position)      : NULL,         \
       elem2    = position ? CADR(position)     : NULL)

ae_obj_t * ae_plist_set(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
  FOR_EACH_PAIR(pair, value, plist) 
    if (CAR(pair) == key) {
      CDR(pair) = value;
      return plist;
    }

  return CONS(key, CONS(value, plist));
}

ae_obj_t * ae_plist_get(ae_obj_t * const plist, ae_obj_t * const key) {
  FOR_EACH_PAIR(pair, value, plist)
    if (CAR(pair) == key)
      return CDR(pair);
  
  return NIL;
}

ae_obj_t * ae_plist_contains_key(ae_obj_t * const plist, ae_obj_t * const key) {
  FOR_EACH_PAIR(pair, value, plist)
    if (CAR(pair) == key)
      return TRUE;

  return NIL;
}
