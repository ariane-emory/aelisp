#include <stdio.h>

#include "ae_list.h"
#include "ae_alist.h"
#include "ae_write.h"
#include "ae_util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_alist_set(ae_obj_t ** list, ae_obj_t * const key, ae_obj_t * const value) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,   "%s setting key", __func__);
  LOG(*list, "in list");
  LOG(value, "to value");
#endif

  if (*list == NULL)
    *list = NIL;
    
  FOR_EACH(elem, *list)
    if (CAR(elem) == key) {
      (CDR(elem) = value);
      goto end;
    }

  *list = CONS(NEW_CONS(key, value), *list);
  
end:

#ifdef AE_LOG_ALIST_PLIST
  LOG(*list, "After  %s:", __func__);
  NL;
#endif
  
  return value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _contains_key
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_alist_contains_key(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,  "%s looking for key:", __func__);
  LOG(list, "in list");
  NL;
#endif

  if (list == NULL || list == NIL) {  
    return false;
  }
    
  FOR_EACH(elem, list)
    if (CAR(elem) == key)
      return true;
  
  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_alist_get(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,  "%s looking for key:", __func__);
  LOG(list, "in list");  
  NL;
#endif

  if (list == NULL || list == NIL)
    return NIL;

  FOR_EACH(elem, list)
    if (CAR(elem) == key)
      return CDR(elem);
  
  return NIL;
}

