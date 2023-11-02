#include <stdio.h>

#include "plist.h"

#include "list.h"
#include "write.h"
#include "log.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// clone_list_up_to helper
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t *clone_list_up_to(ae_obj_t * const pos, ae_obj_t * const list) {
    ae_obj_t *new_list = NIL, *tail = NIL, *cur;
  
    for (cur = list; cur != pos; cur = CDR(cur)) {
        if (cur == NIL) {
            // If cur is NIL, then pos was not part of the list, which is an error.
            // Handle error accordingly, maybe return NIL or log an error message.
            return NIL;
        }
        ae_obj_t *new_elem = CONS(CAR(cur), NIL);
        if (new_list == NIL) {
            new_list = new_elem;
        } else {
            CDR(tail) = new_elem;
        }
        tail = new_elem;
    }
    return new_list;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_immutable
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_plist_set_immutable(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
  assert(plist);
  assert(TAILP(plist));
  assert(NILP(plist) || !(LENGTH(plist) % 2));
  assert(key);
  assert(value);

  if (plist == NIL) {
    // If the list is empty, return a new key-value pair.
    return CONS(key, CONS(value, NIL));
  }
  
  ae_obj_t *pos, *prev = NIL, *new_plist = NIL;
  
  // Search for the key in the plist.
  for (pos = plist; pos != NIL; pos = CDR(CDR(pos))) {
    if (EQL(CAR(pos), key)) {
      // If the key is found, clone the list up to this point and update the value.
      new_plist = clone_list_up_to(pos, plist);
      // Create a new pair with the updated value.
      ae_obj_t *updated_pair = CONS(key, CONS(value, CDDR(pos)));
      // If prev is NIL, the key was in the first pair.
      if (prev == NIL) {
        return updated_pair;
      } else {
        // Otherwise, set the cdr of the last pair of new_plist to updated_pair.
        CDR(prev) = updated_pair;
        return new_plist;
      }
    }
    prev = CDR(pos); // Keep track of the previous pair to be able to link the list.
  }

  // The key was not found; create a new list with the key-value pair at the front.
  return CONS(key, CONS(value, plist));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_mut
////////////////////////////////////////////////////////////////////////////////////////////////////
void ae_plist_set_mut(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value) {
  assert(plist);
  assert(CONSP(plist));
  assert(!(LENGTH(plist) % 2));
  assert(key);
  assert(value);
  
  // Search for the key in the plist.
  for (ae_obj_t *pos = plist; pos != NIL; pos = CDR(CDR(pos))) {
    if (EQL(CAR(pos), key)) {
      // If key is found, just update the value.
      CADR(pos) = value;
      return; // Mutation done; early exit.
    }
  }

  // Key wasn't found, so prepend it by mutating in place.
  ae_obj_t *new_tail = CONS(CAR(plist), CONS(CADR(plist), CDR(CDR(plist))));
  CAR(plist) = key;
  // After setting the new key, we also need to set the new value here.
  CDR(plist) = CONS(value, new_tail); // This line is changed.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
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
