#include <stdio.h>

#include "ae_list.h"
#include "ae_alist.h"
#include "ae_write.h"
#include "ae_util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_alist_set(ae_obj_t ** list, ae_obj_t * const key, ae_obj_t * const value) {
#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key,   "%s setting key", __func__);
  LOG(*list, "in list");
  LOG(value, "to value");
#endif
 
  if (*list == NULL)
    *list = NIL;

  if (*list != NIL)
    FOR_EACH(elem, *list)
      if (CAR(elem) == key) {
        (CDR(elem) = value);
        goto end;
      }

  *list = CONS(NEW_CONS(key, value), *list);
  
end:

#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key,   "after setting key")
  LOG(*list, "list is");
  NL;
#endif

  return value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _contains_key
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_alist_contains_key(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key,  "%s looking for key:", __func__);
  LOG(list, "in list");
#endif

  if (list == NULL || list == NIL) 
    goto failed;
    
  FOR_EACH(elem, list)
    if (CAR(elem) == key) {
#ifdef AE_LOG_ALIST_PLIST_SET_GET
      LOG(key, "found key");
      NL;
#endif

      return true;
    }

failed:

#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key, "did not find");
  NL;
#endif
  
  return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_alist_get(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key,  "%s looking for key:", __func__);
  LOG(list, "in list");  
#endif

  if (list == NULL || list == NIL)
    goto failed;

  FOR_EACH(elem, list) {

#ifdef AE_LOG_ALIST_PLIST_SET_GET
    LOG(elem, "elem");
#endif
    
    if (CAR(elem) == key) {
#ifdef AE_LOG_ALIST_PLIST_SET_GET
      LOG(key,      "found key");
      LOG(CDR(elem), "with value");
      NL;
#endif

      return CDR(elem);
    }
  }
  
failed:

#ifdef AE_LOG_ALIST_PLIST_SET_GET
  LOG(key, "did not find");
  NL;
#endif

  return NIL;
}





























