#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_lisp_car  (ae_obj_t * const args);
ae_obj_t * ae_lisp_cdr  (ae_obj_t * const args);
ae_obj_t * ae_lisp_cons (ae_obj_t * const args);
ae_obj_t * ae_lisp_eq   (ae_obj_t * const args);
ae_obj_t * ae_lisp_eql  (ae_obj_t * const args);
ae_obj_t * ae_lisp_atomp(ae_obj_t * const args);
ae_obj_t * ae_lisp_not  (ae_obj_t * const args);
ae_obj_t * ae_lisp_print(ae_obj_t * const args);
ae_obj_t * ae_lisp_princ(ae_obj_t * const args);
