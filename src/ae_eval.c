#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"
#include "ae_util.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define DISPATCH(table, obj, ...)                                                                  \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                \
    if (table[ix].type == GET_TYPE(obj))                                                           \
      return (*table[ix].handler)(obj, __VA_ARGS__);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch handlers
////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_obj_t * self(ae_obj_t * obj, ae_obj_t * env) {
  (void)env;
  return obj;
}

static ae_obj_t * lookup(ae_obj_t * sym, ae_obj_t * env) {
  return ENV_FIND(env, sym);
}

static ae_obj_t * apply(ae_obj_t * list, ae_obj_t * env) {
  (void)env;
  return ae_apply(CAR(list), env, CDR(list));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply dispatch handlers
////////////////////////////////////////////////////////////////////////////////////////////////////

//==================================================================================================
// apply core funs
//==================================================================================================

static ae_obj_t * apply_core_fun(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
#ifdef AE_LOG_EVAL
  LOG(fun, "[apply core fun %s]", fun->name)
  OLOG(env);
  OLOG(args);
#endif

  ae_obj_t * ret = NIL;

  if (SPECIAL_FUNP(fun)) {
    // special funs get their un-evaluated args, plus the env.
    ae_obj_t * env_and_args = CONS(env, args); 

    ret = (*FUN_VAL(fun))(env_and_args);
  }
  else {
    ae_obj_t * evaled_args = NIL;
    
    FOR_EACH(elem,  args)
      PUSH(evaled_args, EVAL(env, elem));

    ret = (*FUN_VAL(fun))(evaled_args);
  }
  
#ifdef AE_LOG_EVAL
  LOG(ret, "apply core fun %s returns", fun->name);
  NL;
#endif
  
  return ret;
}                                                                               
 
//==================================================================================================
// apply lambda funs
//==================================================================================================

static ae_obj_t * apply_lambda(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
  (void)env;
#ifdef AE_LOG_EVAL
  PR("\n[Apply lambda]\n");
  OLOG(fun);
  LOG(OBJ_PARAMS(fun), "params");
  LOG(OBJ_BODY(fun),   "body");
  OLOG(env);
  OLOG(args);
#endif
  ae_obj_t * new_env = NEW_ENV(OBJ_ENV(fun), OBJ_PARAMS(fun), args);
  ae_obj_t * body    = CONS(INTERN("progn"), OBJ_BODY(fun));
#ifdef AE_LOG_EVAL
  PR("\n[Created exec env]\n");
  LOG(new_env->parent,  "parent");
  LOG(new_env->symbols, "symbols");
  LOG(new_env->values,  "values");
  OLOG(body);
#endif
  ae_obj_t * result = EVAL(new_env, body);
#ifdef AE_LOG_EVAL
  OLOG(result);
#endif
  return result;
}

//==================================================================================================
// _expand_macro
//==================================================================================================

ae_obj_t * expand_macro(ae_obj_t * macro, ae_obj_t * env, ae_obj_t * args) {
  (void)env;
#ifdef AE_LOG_CORE
  PR("\n[Expand macro]\n");
  OLOG(macro);
  LOG(OBJ_PARAMS(macro), "params");
  LOG(OBJ_BODY(macro),   "body");
  OLOG(env);
  OLOG(args);
#endif
  ae_obj_t * new_env = NEW_ENV(OBJ_ENV(macro), OBJ_PARAMS(macro), args);
  ae_obj_t * body    = CONS(INTERN("progn"), OBJ_BODY(macro));
#ifdef AE_LOG_EVAL
  PR("\n[Created expand env]\n");
  LOG(new_env->parent,  "parent");
  LOG(new_env->symbols, "symbols");
  LOG(new_env->values,  "values");
  OLOG(body);
#endif
  ae_obj_t * result = EVAL(new_env, body);
#ifdef AE_LOG_EVAL
  OLOG(result);
#endif
  return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_eval(ae_obj_t * env, ae_obj_t * obj) {  
  ASSERT_ENVP(env);

  DISPATCH(eval_dispatch, obj, env);

  fprintf(stderr, "Don't know how to eval a %s.\n", TYPE_STR(GET_TYPE(obj)));
  assert(0);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////

static const struct { ae_type_t type; ae_obj_t * (*handler)(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args); }
apply_dispatch[] = {
  { AE_MACRO,    expand_macro     },
  { AE_CORE_FUN, &apply_core_fun  },
  { AE_LAMBDA,   &apply_lambda    },
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
#ifdef AE_LOG_EVAL
  LOG(fun,"[Dispatch fun]");
  OLOG(args);
  NL;
#endif

  fun = EVAL(env, fun); 

  ASSERT_FUNP(fun);
  ASSERT_TAILP(args);

  DISPATCH(apply_dispatch, fun, env, args);

  fprintf(stderr, "Don't know how to apply a %s.\n", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
