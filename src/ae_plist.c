#include <stdio.h>

#include "ae_list.h"
#include "ae_plist.h"
#include "ae_write.h"
#include "ae_util.h"

ae_obj_t * ae_plist_set(ae_obj_t ** list, ae_obj_t * const key, ae_obj_t * const value) {
  if (*list == NULL)
    *(ae_obj_t **)list = NIL;

  if (*list != NIL)
    for (ae_obj_t * position = *list; position != NIL; position = CDR(CDR(position))) {
      ae_obj_t * elem1 = CAR(position);
      ae_obj_t * elem2 = position ? CADR(position) : NIL;
      
      if (elem1 == key)
        return CADR(position) = value;
    }

  return (*(ae_obj_t **)list = CONS(key, CONS(value, *list)));
}

ae_obj_t * ae_plist_get(ae_obj_t * const list, ae_obj_t * const key) {
  if (list == NIL)
    return *list;


  for (ae_obj_t * position = *list; position != NIL; position = CDR(CDR(position))) {
    ae_obj_t * elem1 = CAR(position);
    ae_obj_t * elem2 = position ? CADR(position) : NIL;
      
    if (elem1 == key)
      return elem2;
  }
  
  return NIL;
}

bool ae_plist_contains_key(ae_obj_t * const * list, ae_obj_t * const key) {
#ifdef AE_LOG_PLIST
  PR("%s got list %8p.\n", __func__, list);
#endif
  
  if (list == NULL || *list == NUL)
    return false;
  
    *(ae_obj_t **)list = NIL;
    
  for (ae_obj_t * position = *list; position != NIL; position = CDR(CDR(position)))
    if (CAR(position) == key)
      return true;

  return false;
}
