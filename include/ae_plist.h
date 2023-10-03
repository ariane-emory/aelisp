#pragma once

#include <stdbool.h>

#define PSET(alist, key, value)        (ae_plist_set (&(alist), (key), (value)))
#define PGET(alist, key)               (ae_plist_get (&(alist), (key)))
#define PHAS(alist, key)               (ae_plist_contains_key(&(alist), (key)))

struct ae_obj_t * ae_plist_set         (struct ae_obj_t **      plist, struct ae_obj_t * const key, struct ae_obj_t * const value);
struct ae_obj_t * ae_plist_get         (struct ae_obj_t * const plist, struct ae_obj_t * const key);
bool              ae_plist_contains_key(struct ae_obj_t * const plist, struct ae_obj_t * const key);
