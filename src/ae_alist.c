#include "ae_list.h"
#include "ae_alist.h"
#include "ae_util.h"

ae_obj_t * ae_alist_set(ae_obj_t ** list, ae_obj_t * const key, ae_obj_t * const value) {
  if (*list == NULL)
    *(ae_obj_t **)list = NIL;

  FOR_EACH(elem, *list)    
    if (CAR(elem) == key) {
      return (CDR(elem) = value);
  
  return *list = CONS(NEW_CONS(key, value), *list);
}

ae_obj_t * ae_alist_get(ae_obj_t * const * list, ae_obj_t * const key) {
  if (list == NULL)
    return NIL;

  if (list == NIL)
    return NIL;

  FOR_EACH(elem, *list)
    if (CAR(elem) == key)
      return CDR(elem);
  
  return NIL; 
}

bool ae_alist_contains_key(ae_obj_t * const * list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST
  PR("%s got list %8p.\n", __func__, list);
#endif

  if (list == NULL)
    return false;

  if (list == NIL)
    return false;
    
  FOR_EACH(elem, *list)
    if (CAR(elem) == key)
      return true;
  
  return false; 
}

