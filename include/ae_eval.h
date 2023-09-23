#pragma once

#include "ae_obj.h"

#define EVAL(env, obj)   (ae_eval((env), (obj)))
#define APPLY(fun, args) (ae_apply((fun), (args)))

////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_eval (ae_obj_t * env, ae_obj_t * obj);
ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * args, ae_obj_t * env);
