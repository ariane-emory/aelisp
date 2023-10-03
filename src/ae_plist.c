#include <stdio.h>

#include "ae_list.h"
#include "ae_plist.h"
#include "ae_write.h"
#include "ae_util.h"

#define FOR_EACH_PAIR(elem1, elem2, list)                       \
  for (ae_obj_t * position = (list),                            \
                * elem1    = CAR(position),                     \
                * elem2    = position ? CADR(position) : NULL;  \
       elem1 && elem2;                                          \
       position = position ? CDR(CDR(position)) : NULL,         \
       elem1    = position ? CAR(position)      : NULL,         \
       elem2    = position ? CADR(position)     : NULL)

ae_obj_t * ae_plist_set(ae_obj_t ** plist, ae_obj_t * const key, ae_obj_t * const value) {
  if (*plist == NULL)
    *(ae_obj_t **)plist = NIL;

  if (*plist != NIL)
    for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position))) {
      /* WRITE(position); NL; FF; */
      
      ae_obj_t * elem1 = CAR(position);
      ae_obj_t * elem2 = position ? CADR(position) : NIL;
      
      /* OLOG(elem1); */
      /* OLOG(elem2); */
      
      if (elem1 == key) {
        // PR("Found it.");

        return CADR(position) = value;
      }
    }

  return (*(ae_obj_t **)plist = CONS(key, CONS(value, *plist)));
}

ae_obj_t * ae_plist_get(ae_obj_t * const * plist, ae_obj_t * const key) {
  if (*plist == NULL)
    *(ae_obj_t **)plist = NIL;

  if (*plist == NIL)
    return *plist;

  //OLOG(*plist);

  for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position))) {
    /* OLOG(position); SPC; PUT(position); NL; FF; */

    ae_obj_t * elem1 = CAR(position);
    ae_obj_t * elem2 = position ? CADR(position) : NIL;

    /* OLOG(elem1); */
    /* OLOG(elem2); */
      
    if (elem1 == key)
      return elem2;
  }
  
  return NIL;
}

bool ae_plist_contains_key(ae_obj_t * const * plist, ae_obj_t * const key) {
  if (*plist == NULL)
    return false;  // Return false if plist is NULL

  if (*plist == NIL) {
    /* PR("plist is NIL\n"); */
    
    return false;
  }

  for (ae_obj_t * position = *plist; position != NIL; position = CDR(CDR(position))) {
    ae_obj_t * elem1 = CAR(position);
    ae_obj_t * elem2 = position ? CADR(position) : NIL;

    /* OLOG(elem1); */
    /* OLOG(elem2); */

    if (elem1 == key) {
      /* PR("found it."); */
      
      return true;
    }
  }

  return false;
}
