#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"

#define NL (putchar('\n'))
#define PR(...) (fprintf(stdout, __VA_ARGS__))
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

#define DISPATCH(table, obj, args)                                                                 \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                \
    if (table[ix].type == GET_TYPE(obj))                                                           \
      return (*table[ix].handler)(obj, args);

////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_obj_t * self(ae_obj_t * obj, ae_obj_t * env) {
  (void)env;
  return obj;
}

static ae_obj_t * lookup(ae_obj_t * sym, ae_obj_t * env) {
  return ENV_FIND(env, sym);
}

static ae_obj_t * apply(ae_obj_t * list, ae_obj_t * env) {
  return ae_apply(CAR(list), CDR(env));
}

static const struct { ae_type_t type; ae_obj_t * (*handler)(ae_obj_t *, ae_obj_t *); }
eval_dispatch[] = {
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

static const struct { ae_type_t type; ae_obj_t * (*handler)(ae_obj_t *, ae_obj_t *); }
apply_dispatch[] = {
  { AE_CORE_FUN, &apply_core_fun },
  { AE_LAMBDA,   &apply_lambda   },
};

ae_obj_t * ae_eval(ae_obj_t * obj, ae_obj_t * env) {
  PR("Eval ");
  WRITE(obj);
  PR(" in ");
  WRITE(env);
  NL;
  
  ASSERT_ENVP(env);
  DISPATCH(eval_dispatch, obj, env);
  fprintf(stderr, "Don't know how to eval a %s.\n", TYPE_STR(GET_TYPE(obj)));
  assert(0);
}

ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * args) {
  PR("Apply ");
  WRITE(fun);
  PR(" to ");
  WRITE(args);
  NL;
  
  ASSERT_FUNP(fun);
  ASSERT_TAILP(args);
  DISPATCH(apply_dispatch, fun, args);
  fprintf(stderr, "Don't know how to apply a %s.\n", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
