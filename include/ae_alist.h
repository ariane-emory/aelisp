#pragma once

#include <stdbool.h>

#include "ae_obj.h"

#define ASET(alist, key, value)  (ae_alist_set (&(alist), (key), (value)))
#define AGET(alist, key)         (ae_alist_get (&(alist), (key)))
#define AHAS(alist, key)         (ae_alist_contains_key(&(alist), (key)))

ae_obj_t * ae_alist_set          (ae_obj_t ** const   alist, ae_obj_t * const key, ae_obj_t * const value);
ae_obj_t * ae_alist_get          (ae_obj_t *  const * alist, ae_obj_t * const key);
bool       ae_alist_contains_key (ae_obj_t *  const * alist, ae_obj_t * const key);

