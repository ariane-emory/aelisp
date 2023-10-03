#pragma once

#include <stdbool.h>

#define ASET(alist, key, value)         (ae_alist_set (&(alist), (key), (value)))
#define AGET(alist, key)                (ae_alist_get (&(alist), (key)))
#define AHAS(alist, key)                (ae_alist_contains_key(&(alist), (key)))

struct ae_obj_t * ae_alist_set          (struct ae_obj_t **      alist, struct ae_obj_t * const key, struct ae_obj_t * const value);
struct ae_obj_t * ae_alist_get          (struct ae_obj_t * const alist, struct ae_obj_t * const key);
bool              ae_alist_contains_key (struct ae_obj_t * const alist, struct ae_obj_t * const key);

