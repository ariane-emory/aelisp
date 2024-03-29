#pragma once

#include <stdbool.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// forward declarations
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct ae_obj_t ae_obj_t;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// types
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct ae_plist_split_around_kvp_t {
  ae_obj_t * before_kvp;
  ae_obj_t * after_kvp;
} ae_plist_split_around_kvp_t;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define PHAS                ae_plist_contains_key
#define PGET                ae_plist_get
#define PSET_INTERNAL       ae_plist_set_internal
#define PSET_MUTATING       ae_plist_set_mutating
#define PSET_NONMUTATING    ae_plist_set_nonmutating
#define PREMOVE_MUTATING    ae_plist_remove_mutating
#define PREMOVE_NONMUTATING ae_plist_remove_nonmutating
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// object 'properties' macros:
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define HAS_PROP(key, obj)               (PHAS(PROPS((obj)), KW(key)))
#define HAS_PROP_RAW(key, obj)           (PHAS(PROPS((obj)), (key)))
#define GET_PROP(key, obj)               (PGET(PROPS((obj)), KW(key)))
#define GET_PROP_RAW(key, obj)           (PGET(PROPS((obj)), (key))) 
#define PUT_PROP(val, key, obj)          ({ CAPTURE(obj); PROPS(CAPTURED) = (PSET_INTERNAL(PROPS(CAPTURED), KW(key), (val))); })
#define PUT_PROP_RAW(val, key, obj)      ({ CAPTURE(obj); PROPS(CAPTURED) = (PSET_INTERNAL(PROPS(CAPTURED),   (key), (val))); })
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// functions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_plist_split_around_kvp_t ae_plist_split_around_kvp  (ae_obj_t * const plist, ae_obj_t * const key                          );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool                        ae_plist_contains_key      (ae_obj_t * const plist, ae_obj_t * const key                          );
ae_obj_t *                  ae_plist_get               (ae_obj_t * const plist, ae_obj_t * const key                          );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void                        ae_plist_remove_mutating   (ae_obj_t * const plist, ae_obj_t * const key                          );
ae_obj_t *                  ae_plist_remove_nonmutating(ae_obj_t * const plist, ae_obj_t * const key                          );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *                  ae_plist_set_internal      (ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value  );
void                        ae_plist_set_mutating      (ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value  );
ae_obj_t *                  ae_plist_set_nonmutating   (ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value  );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
