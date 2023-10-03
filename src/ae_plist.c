#include <stdio.h>

#include "ae_list.h"
#include "ae_plist.h"
#include "ae_write.h"
#include "ae_util.h"

ae_obj_t * ae_plist_set(ae_obj_t ** list, ae_obj_t * const key, ae_obj_t * const value) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,   "%s setting key", __func__);
  LOG(*list, "in list");
  LOG(value, "to value");
#endif

  if (*list == NULL)
    *(ae_obj_t **)list = NIL;

  if (*list != NIL)
    for (ae_obj_t * position = *list; position != NIL; position = CDR(CDR(position))) {
      ae_obj_t    * elem1    = CAR(position);
      ae_obj_t    * elem2    = position ? CADR(position) : NIL;
      
      if (elem1 == key)
        return CADR(position) = value;
    }

  *list = CONS(key, CONS(value, *list));
  
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,   "after setting key")
  LOG(*list, "list is");
  NL;
#endif

  return value;
}

ae_obj_t * ae_plist_get(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,  "%s looking for key", __func__);
  LOG(list, "in list");
#endif

  if (list == NULL || list == NIL)
    goto end;

  for (ae_obj_t * position = list; position != NIL; position = CDR(CDR(position))) {
    ae_obj_t    * elem1    = position;
    ae_obj_t    * elem2    = position ? CADR(position) : NIL;
    
    if (elem1 == key) {
#ifdef AE_LOG_ALIST_PLIST
      LOG(key,   "found key");
      LOG(elem2, "with value");
      NL;
#endif

      return elem2;
    }
  }

end:
#ifdef AE_LOG_ALIST_PLIST
  LOG(key, "did not find");
  NL;
#endif

  return NIL;
}

bool ae_plist_contains_key(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_ALIST_PLIST
  LOG(key,  "%s looking for key", __func__);
  LOG(list, "in list");
#endif

  if (list == NULL || list == NIL)
    goto end;

  for (ae_obj_t * position = list;
       position != NIL;
       position = CDR(CDR(position)))
    if (CAR(position) == key) {
#ifdef AE_LOG_ALIST_PLIST
      LOG(key, "found key");
      NL;
#endif
      
      return true;
    }

end:
#ifdef AE_LOG_ALIST_PLIST
  LOG(key, "did not find");
  NL;
#endif
  
  return false;
}
