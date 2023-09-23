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

static ae_obj_t * apply(ae_obj_t * obj, ae_obj_t * env) {
  return ENV_FIND(env, obj);
}

static const struct { ae_type_t type; ae_obj_t * (*func)(ae_obj_t *, ae_obj_t *); } eval_dispatch[] = {
  { AE_INTEGER,  &self           },
  { AE_RATIONAL, &self           },
  { AE_FLOAT,    &self           },
  { AE_INF,      &self           },
  { AE_CHAR,     &self           },
  { AE_STRING,   &self           },
  { AE_LAMBDA,   &self           },
  { AE_CORE_FUN, &self           },
  { AE_SYMBOL,   &lookup         },
  { AE_CONS,     &apply          },
};

static ae_obj_t * apply_core_fun(ae_obj_t * fun, ae_obj_t * args) {
  (void)fun, (void)args; assert(0); // not yet implemented
}

static ae_obj_t * apply_lambda(ae_obj_t * fun, ae_obj_t * args) {
  (void)fun, (void)args; assert(0); // not yet implemented
}

static const struct { ae_type_t type; ae_obj_t * (*func)(ae_obj_t *, ae_obj_t *); } apply_dispatch[] = {
  { AE_CORE_FUN, &apply_core_fun },
  { AE_LAMBDA,   &apply_lambda   },
};

#define DISPATCH(table, thing, args)                                                                \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                 \
    if (table[ix].type == GET_TYPE(thing))                                                          \
      return (*table[ix].func)(thing, args);


ae_obj_t * ae_eval(ae_obj_t * obj, ae_obj_t * env) {
  ASSERT_ENVP(env);

  DISPATCH(eval_dispatch, obj, env);
  /* for (size_t ix = 0; ix < ARRAY_SIZE(eval_dispatch); ix++) */
  /*   if (eval_dispatch[ix].type == GET_TYPE(obj)) */
  /*     return (*eval_dispatch[ix].func)(obj, env); */

  fprintf(stderr, "Don't know how to eval a %s.\n", TYPE_STR(GET_TYPE(obj)));
  assert(0);
}

ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * args) {
  ASSERT_FUNP(fun);
  ASSERT_TAILP(args);
  
  for (size_t ix = 0; ix < ARRAY_SIZE(apply_dispatch); ix++)
    if (apply_dispatch[ix].type == GET_TYPE(fun))
      return (*apply_dispatch[ix].func)(fun, args);

  fprintf(stderr, "Don't know how to apply a %s.\n", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
