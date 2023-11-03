#include <stdio.h>

#include "plist.h"

#include "list.h"
#include "write.h"
#include "log.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
//  split_list_at_value helper
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_nonmutating
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_plist_set_nonmutating(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
  assert(plist);
  assert(key);
  assert(value);
  assert(TAILP(plist));
  assert(NILP(plist) || !(LENGTH(plist) % 2));

  ae_obj_t * const new_kvp = CONS(key, CONS(value, NIL));
  
  if (NILP(plist))
    return new_kvp;

  ae_plist_split_around_kvp_t split = ae_plist_split_around_kvp(key, plist);

  return ae_list_join3(split.before_kvp, new_kvp, split.after_kvp);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_nonmutating
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_plist_remove_nonmutating(ae_obj_t * const plist, ae_obj_t * const key) {
  assert(plist);
  assert(key);
  assert(TAILP(plist));
  assert(NILP(plist) || !(LENGTH(plist) % 2));

  if (NILP(plist))
    return NIL;

  ae_plist_split_around_kvp_t split = ae_plist_split_around_kvp(key, plist);

  return ae_list_join3(split.before_kvp, NIL, split.after_kvp);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_mutating
////////////////////////////////////////////////////////////////////////////////////////////////////
void ae_plist_set_mutating(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
  assert(plist);
  assert(CONSP(plist));
  assert(!(LENGTH(plist) % 2));
  assert(key);
  assert(value);
  
  for (ae_obj_t * pos = plist; pos != NIL; pos = CDR(CDR(pos)))
    if (EQL(CAR(pos), key)) {
      CADR(pos) = value;

      return; 
    }

  ae_obj_t * const new_tail = CONS(CAR(plist), CONS(CADR(plist), CDR(CDR(plist))));
  CAR(plist) = key;
  CDR(plist) = CONS(value, new_tail);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_internal: I don't fully trust this one yet.
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_plist_set_internal(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
    if (plist == NULL || plist == NIL) {
        // Use the immutable version if the list is empty
        // as this will create a new list with the key-value pair.
        return ae_plist_set_nonmutating(NIL, key, value);
    } else {
        // Use the mutable version to update the existing list.
        ae_plist_set_mutating(plist, key, value);
        return plist; // Return the updated plist.
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _set (old version, kept because we might switch propperty setting back to using it)
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_plist_set(ae_obj_t * list, ae_obj_t * const key, ae_obj_t * const value) {
#ifdef AE_LOG_KVP_SET_GET
  LOG(key,   "%s setting key", __func__);
  LOG(list,  "in list");
  LOG(value, "to value");
#endif
  
  assert(!list || (TAILP(list) && ! (LENGTH(list) % 2)));
  
  if (list == NULL)
    list = NIL;

  if (list != NIL)
    for (ae_obj_t * position = list; position != NIL; position = CDR(CDR(position))) {
      ae_obj_t    * elem1    = CAR(position);
      ae_obj_t    * elem2    = position ? CADR(position) : NIL;
      
      if (EQL(elem1, key)) {
        CADR(position) = value;

        goto end;
      }
    }

  list = CONS(key, CONS(value, list));

end:

#ifdef AE_LOG_KVP_SET_GET
  LOG(key,   "after setting key");
  LOG(list, "list is");
  NL;
#endif

  return list;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _contains_key
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_plist_contains_key(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_KVP_HAS
  LOG(key,  "%s looking for key", __func__);
  LOG(list, "in list");
#endif

  if (list == NULL || list == NIL)
    goto failed;

  for (ae_obj_t * position = list;
       position && position != NIL;
       position  = CDR(CDR(position)))
    if (EQL(CAR(position), key)) {
#ifdef AE_LOG_KVP_HAS
      LOG(key, "found key");
      NL;
#endif
      
      return true;
    }

failed:

#ifdef AE_LOG_KVP_HAS
  LOG(key, "did not find");
  NL;
#endif
  
  return false;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _get
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_plist_get(ae_obj_t * const list, ae_obj_t * const key) {
#ifdef AE_LOG_KVP_SET_GET
  LOG(key,  "%s looking for key", __func__);
  LOG(list, "in list");
#endif

  if (list == NULL || list == NIL)
    goto failed;

  for (ae_obj_t * position = list;
       position && position != NIL;
       position  = CDR(CDR(position))) {

    assert(CDR(CDR(position)));
    
    ae_obj_t     * elem1 = CAR(position);
    ae_obj_t     * elem2 = position ? CADR(position) : NIL;

#ifdef AE_LOG_KVP_SET_GET
    LOG(position, "position");
    LOG(elem1,    "elem1");
    if (elem2)
      LOG(elem2,  "elem2");
#endif
    
    if (EQL(CAR(position), key)) {
#ifdef AE_LOG_KVP_SET_GET
      LOG(key,   "found key");
      LOG(elem2, "with value");
      NL;
#endif

      return elem2;
    }
  }

failed:

#ifdef AE_LOG_KVP_SET_GET
  LOG(key, "did not find");
  NL;
#endif

  return NIL;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _split_around_kvp
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_plist_split_around_kvp_t ae_plist_split_around_kvp(ae_obj_t * const key, ae_obj_t * const plist) {
  assert(key);
  assert(plist);
  assert(TAILP(plist));

  if (NILP(plist))
    return (ae_plist_split_around_kvp_t){ NIL, NIL };
  
  ae_obj_t * new_front         = NIL;
  ae_obj_t * new_front_tailtip = NULL; 
  ae_obj_t * after_kvp         = plist;   

  while (CONSP(after_kvp) && !EQL(CAR(after_kvp), key)) {
    if (new_front_tailtip == NULL) {
      new_front = CONS(CAR(after_kvp), NIL);
      new_front_tailtip = new_front;
    } else {
      CDR(new_front_tailtip) = CONS(CAR(after_kvp), NIL);
      new_front_tailtip = CDR(new_front_tailtip); 
    }
    
    after_kvp = CDR(after_kvp); 
  }

  if (CONSP(after_kvp)) {
    after_kvp = CDDR(after_kvp); 
  } else {
    new_front = NIL;
    after_kvp = plist;
  }

  return (ae_plist_split_around_kvp_t){ new_front, after_kvp };
}
////////////////////////////////////////////////////////////////////////////////////////////////////
