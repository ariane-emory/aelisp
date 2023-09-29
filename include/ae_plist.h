#pragma once

#include "ae_obj.h"
#include "ae_list.h"

 
ae_obj_t * ae_plist_set(ae_obj_t * const plist, ae_obj_t * const key, ae_obj_t * const value);
ae_obj_t * ae_plist_get(ae_obj_t * const plist, ae_obj_t * const key);
ae_obj_t * ae_plist_contains_key(ae_obj_t * const plist, ae_obj_t * const key);
