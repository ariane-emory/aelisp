#include <stdio.h>

#include "ae_list.h"
#include "ae_plist.h"
#include "ae_write.h"
#include "ae_util.h"

ae_obj_t * ae_plist_set(ae_obj_t ** plist, ae_obj_t * const key, ae_obj_t * const value) {
  if (*plist == NULL)
    *(ae_obj_t **)plist = NIL;

  if (*plist != NIL)
    for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position))) {
      ae_obj_t * elem1 = CAR(position);
      ae_obj_t * elem2 = position ? CADR(position) : NIL;
      
      if (elem1 == key)
        return CADR(position) = value;
    }

  return (*(ae_obj_t **)plist = CONS(key, CONS(value, *plist)));
}

ae_obj_t * ae_plist_get(ae_obj_t * const * plist, ae_obj_t * const key) {
  if (*plist == NULL)
    *(ae_obj_t **)plist = NIL;

  if (*plist == NIL)
    return *plist;


  for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position))) {
    ae_obj_t * elem1 = CAR(position);
    ae_obj_t * elem2 = position ? CADR(position) : NIL;
      
    if (elem1 == key)
      return elem2;
  }
  
  return NIL;
}

bool ae_plist_contains_key(ae_obj_t * const * plist, ae_obj_t * const key) {
  if (*plist == NULL)
    return false;  

  if (*plist == NIL)      
    return false;

  for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position)))
    if (CAR(position) == key)
      return true;

  return false;
}
