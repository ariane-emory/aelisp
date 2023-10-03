#pragma once

#include <stdbool.h>

#include "ae_obj.h"
#include "ae_list.h"

#define PSET(alist, key, value)  (ae_plist_set (&(alist), (key), (value)))
#define PGET(alist, key)         (ae_plist_get (&(alist), (key)))
#define PHAS(alist, key)         (ae_plist_contains_key(&(alist), (key)))

ae_obj_t * ae_plist_set(         ae_obj_t **        plist, ae_obj_t * const key, ae_obj_t * const value);
ae_obj_t * ae_plist_get(         ae_obj_t * const * plist, ae_obj_t * const key);
bool       ae_plist_contains_key(ae_obj_t * const * plist, ae_obj_t * const key);
