#pragma once

#include "ae_obj.h"

ae_obj_t * ae_alist_set          (ae_obj_t * const alist, ae_obj_t * const key, ae_obj_t * const value);
ae_obj_t * ae_aelist_get         (ae_obj_t * const alist, ae_obj_t * const key);
ae_obj_t * ae_aelist_contains_key(ae_obj_t * const alist, ae_obj_t * const key);

