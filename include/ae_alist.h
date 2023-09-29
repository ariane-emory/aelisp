#pragma once

#include "ae_obj.h"

#define A_SET(alist, key, value) (ae_alist_set (&(alist), (key), (value)))
#define A_GET(alist, key)        (ae_alist_get ((alist), (key)))
#define A_CONTAINS(alist, key)   (ae_alist_contains_key((alist), (key)))

ae_obj_t * ae_alist_set          (ae_obj_t ** const alist, ae_obj_t * const key, ae_obj_t * const value);
ae_obj_t * ae_aelist_get         (ae_obj_t *  const alist, ae_obj_t * const key);
ae_obj_t * ae_aelist_contains_key(ae_obj_t *  const alist, ae_obj_t * const key);

