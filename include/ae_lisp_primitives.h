#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_car (ae_obj_t * const args);
ae_obj_t * ae_lisp_cdr (ae_obj_t * const args);
ae_obj_t * ae_lisp_cons(ae_obj_t * const args);
