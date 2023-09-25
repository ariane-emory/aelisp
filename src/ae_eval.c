#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"
#include "ae_util.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define DISPATCH(row, table, obj)                                                                  \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                \
    if (table[ix].type == GET_TYPE(obj)) {                                                         \
      row = table[ix];                                                                             \
      break;                                                                                       \
}

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
  LOG(fun, "[apply core %s]", fun->name)
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
  LOG(ret, "apply core %s ret", fun->name);
  NL;
#endif
  
  return ret;
}                                                                               
 
//==================================================================================================
// apply lambda fun
//==================================================================================================

static ae_obj_t * apply_user_fun(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
  (void)env;

#ifdef AE_LOG_EVAL
  PR("\n[Apply lambda]");
  OLOG(fun);
  LOG(OBJ_PARAMS(fun), "params");
  LOG(OBJ_BODY(fun),   "body");
  OLOG(env);
  OLOG(args);
#endif
  
  ae_obj_t * evaled_args = NIL;
    
  FOR_EACH(elem,  args)
    PUSH(evaled_args, EVAL(env, elem));

  ae_obj_t * new_env = NEW_ENV(OBJ_ENV(fun), OBJ_PARAMS(fun), evaled_args);
  ae_obj_t * body    = CONS(INTERN("progn"), OBJ_BODY(fun));
  
#ifdef AE_LOG_EVAL
  PR("\n[Created exec env]");
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

typedef struct {
    ae_type_t type;
    ae_obj_t *(*handler)(ae_obj_t *, ae_obj_t *);
} eval_dispatch_t;

static const eval_dispatch_t eval_dispatch[] = {
    {AE_INTEGER,  &self},
    {AE_RATIONAL, &self},
    {AE_FLOAT,    &self},
    {AE_INF,      &self},
    {AE_CHAR,     &self},
    {AE_STRING,   &self},
    {AE_LAMBDA,   &self},
    {AE_CORE_FUN, &self},
    {AE_SYMBOL,   &lookup},
    {AE_CONS,     &apply},
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_eval(ae_obj_t * env, ae_obj_t * obj) {  
  ASSERT_ENVP(env);

  eval_dispatch_t dispatch = {0};
  
  DISPATCH(dispatch, eval_dispatch, obj);

  return (*dispatch.handler)(obj, env);
  
  fprintf(stderr, "Don't know how to eval a %s.\n", TYPE_STR(GET_TYPE(obj)));
  assert(0);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    ae_type_t type;
    bool special;
    ae_obj_t *(*handler)(ae_obj_t *, ae_obj_t *, ae_obj_t *);
} apply_dispatch_t;

static const apply_dispatch_t apply_dispatch[] = {
    { AE_CORE_FUN, false, &apply_core_fun }, // 2nd param is ignored by apply_core_fun.
    { AE_LAMBDA,   true,  &apply_user_fun },
    { AE_MACRO,    false, &apply_user_fun },
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

  apply_dispatch_t dispatch = {0};
  
  DISPATCH(dispatch, apply_dispatch, fun);

  return (*dispatch.handler)(fun, env, args);
  
  fprintf(stderr, "Don't know how to apply a %s.\n", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
