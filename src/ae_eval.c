#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

static ae_obj_t * self(ae_obj_t * obj, ae_obj_t * env) {
  (void)env;
  return obj;
}

static ae_obj_t * lookup(ae_obj_t * obj, ae_obj_t * env) {
  return ENV_FIND(env, obj);
}

static const struct { ae_type_t type; ae_obj_t * (*func)(ae_obj_t *, ae_obj_t *); } eval_dispatch[] = {
  { AE_INTEGER,  &self   },
  { AE_RATIONAL, &self   },
  { AE_FLOAT,    &self   },
  { AE_CHAR,     &self   },
  { AE_STRING,   &self   },
  { AE_STRING,   &lookup },
};

ae_obj_t * ae_eval(ae_obj_t * obj, ae_obj_t * env) {
  ASSERT_ENVP(env);
  
  for (size_t ix = 0; ix < ARRAY_SIZE(eval_dispatch); ix++)
    if (eval_dispatch[ix].type == GET_TYPE(obj))
      return (*eval_dispatch[ix].func)(obj, env);

  fprintf(stderr, "Don't know how to eval a %s.\n", TYPE_STR(GET_TYPE(obj)));
  assert(0);
}
