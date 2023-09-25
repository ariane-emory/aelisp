#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"
#include "ae_util.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define GET_DISPATCH(row, table, obj)                                                              \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                \
    if (table[ix].type == GET_TYPE(obj)) {                                                         \
      row = table[ix];                                                                             \
      break;                                                                                       \
    }

#define MAYBE_EVAL(special, args)                                                                  \
  if (! special) {                                                                                 \
    ae_obj_t * evaled_args = NIL;                                                                  \
    FOR_EACH(elem, args)                                                                           \
      PUSH(evaled_args, EVAL(env, elem));                                                          \
    args = evaled_args;                                                                            \
  }

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch handlers
////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_obj_t * self(ae_obj_t * obj, ae_obj_t * env) {
  (void)env;
#ifdef AE_LOG_EVAL
  LOG(obj, "rtrn self =>");
#endif
  
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

ae_obj_t * apply_core_fun(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
#ifdef AE_LOG_EVAL
  PR("\n\n[apply core %s]", fun->name);  // extra spaces needed here to line up for some reason.
  LOG(fun, "apply core fun");
  LOG(env, "apply core env");
  LOG(args, "apply core args");
#endif

  MAYBE_EVAL(SPECIAL_FUNP(fun), args);
  ae_obj_t * ret = SPECIAL_FUNP(fun)
    ? (*FUN_VAL(fun))(CONS(env, args))
    : (*FUN_VAL(fun))(args);
  
#ifdef AE_LOG_EVAL
  LOG(ret, "<= appl core %s", fun->name);
#endif
  
  return ret;
}                                                                               
 
//==================================================================================================
// apply lambda fun
//==================================================================================================

ae_obj_t * apply_user_fun(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
  (void)env;

#ifdef AE_LOG_EVAL
  PR("\n\n[apply user fun]");
  LOG(fun, "appl user fun fun");
  LOG(OBJ_PARAMS(fun), "appl user fun params");
  LOG(OBJ_BODY(fun), "appl user fun body");
  LOG(env, "appl user fun env");
  LOG(args, "appl user fun args");
#endif
  
  ae_obj_t * new_env = NIL;

  if (SYMBOLP(OBJ_PARAMS(fun)))
    new_env = NEW_ENV(OBJ_ENV(fun), CONS(OBJ_PARAMS(fun), NIL), CONS(args, NIL));
  else
    new_env = NEW_ENV(OBJ_ENV(fun), OBJ_PARAMS(fun), args);
  
  ae_obj_t * body    = CONS(SYM("progn"), OBJ_BODY(fun));
  
#ifdef AE_LOG_EVAL
  PR("\n\n[created exec env]");
  LOG(new_env->parent,  "exec env parent");
  LOG(new_env->symbols, "exec env symbols");
  LOG(new_env->values,  "exec env values");
  LOG(body, "exec env body");
#endif
  
  ae_obj_t * result = EVAL(new_env, body);

#ifdef AE_LOG_EVAL
  LOG(result, "<= user fun");
#endif

  return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    ae_type_t type;
    ae_obj_t * (*handler)(ae_obj_t *, ae_obj_t *);
} eval_dispatch_t;

static const eval_dispatch_t eval_dispatch[] = {
    {AE_INTEGER,  &self},
    {AE_RATIONAL, &self},
    {AE_FLOAT,    &self},
    {AE_INF,      &self},
    {AE_CHAR,     &self},
    {AE_STRING,   &self},
    {AE_LAMBDA,   &self},
    {AE_MACRO,    &self},
    {AE_CORE,     &self},
    {AE_SYMBOL,   &lookup},
    {AE_CONS,     &apply},
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_eval(ae_obj_t * env, ae_obj_t * obj) {  
#ifdef AE_LOG_EVAL
  PR("\n\n[dispatching eval...]");
  LOG(obj, "disp eval obj");
  LOG(env, "disp eval env");
#endif

  ASSERT_ENVP(env);

  eval_dispatch_t dispatch = {0};
  
  GET_DISPATCH(dispatch, eval_dispatch, obj);

#ifdef AE_LOG_EVAL
  PR("\n=> dispatching eval to %s", TYPE_STR(dispatch.type));
#endif

  assert(*dispatch.handler);
  
  return (*dispatch.handler)(obj, env);
  
#ifdef AE_LOG_EVAL
  fprintf(stderr, "\nDon't know how to eval a %s.", TYPE_STR(GET_TYPE(obj)));
#endif
  assert(0);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
    ae_type_t type;
    bool special;
    ae_obj_t * (*handler)(ae_obj_t *, ae_obj_t *, ae_obj_t *);
} apply_dispatch_t;

static const apply_dispatch_t apply_dispatch[] = {
    { AE_CORE,   true,  &apply_core_fun }, // 2nd arg may be ignored internally by apply_core_fun.
    { AE_LAMBDA, false, &apply_user_fun },
    { AE_MACRO,  true,  &apply_user_fun },
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
#ifdef AE_LOG_EVAL
  PR("\n\n[dispatching apply...]");
  LOG(fun, "disp appl fun");
  LOG(args, "disp appl args");
  LOG(env, "disp appl env");
#endif

  fun = EVAL(env, fun); 

#ifdef AE_LOG_EVAL
  LOG(fun, "FUN");
#endif
  
  ASSERT_FUNP(fun);
  ASSERT_TAILP(args);

  apply_dispatch_t dispatch = {0};
  
  GET_DISPATCH(dispatch, apply_dispatch, fun);

#ifdef AE_LOG_EVAL
  PR("\n=> dispatching apply to %s", TYPE_STR(dispatch.type));
#endif
  
  MAYBE_EVAL(dispatch.special, args);
  ae_obj_t * ret = dispatch.special
    ? (*dispatch.handler)(fun, env, args)
    : (*dispatch.handler)(fun, env, args);

  return ret;
  
  fprintf(stderr, "\nDon't know how to apply a %s.", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
 
