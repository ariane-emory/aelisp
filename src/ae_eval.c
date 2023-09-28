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

#ifdef AE_EVAL_EARLY_RETURN_ON_ERROR
#  define MAYBE_EVAL_AND_BAIL_ON_ERROR(special, args)                                              \
  if (! special) {                                                                                 \
    ae_obj_t * evaled_args = NIL;                                                                  \
    FOR_EACH(elem, args) {                                                                         \
      ae_obj_t * tmp = EVAL(env, elem);                                                            \
      if (ERRORP(tmp))                                                                             \
      {                                                                                            \
        args = tmp;                                                                                \
        break;                                                                                     \
      }                                                                                            \
      PUSH(evaled_args, tmp);                                                                      \
    }                                                                                              \
    if (ERRORP(args)) {                                                                            \
      return args;                                                                                 \
    }                                                                                              \
    args = evaled_args;                                                                            \
  }
#else
#  define MAYBE_EVAL_AND_BAIL_ON_ERROR(special, args)                                              \
  if (! special) {                                                                                 \
    ae_obj_t * evaled_args = NIL;                                                                  \
    FOR_EACH(elem, args)                                                                           \
    {                                                                                              \
      ae_obj_t * tmp = EVAL(env, elem);                                                            \
      PUSH(evaled_args, tmp);                                                                      \
    }                                                                                              \
    args = evaled_args;                                                                            \
  }
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch handlers
////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_obj_t * self(ae_obj_t * obj, ae_obj_t * env) {
  (void)env;

#ifdef AE_LOG_EVAL
  LOG(obj, "<= rtrn self");
#endif
  
  return obj;
}

static ae_obj_t * lookup(ae_obj_t * sym, ae_obj_t * env) {
  ae_obj_t * ret = KEYWORDP(sym)
    ? sym
    : ENV_FIND(env, sym);

#ifdef AE_LOG_EVAL
  if (LAMBDAP(ret) || MACROP(ret) || COREP(ret)) {
    LOG(ret, "looked up:    ");
  }
  LOG(ret, "<= rtrn lookup");
#endif

  return ret;
}

static ae_obj_t * apply(ae_obj_t * list, ae_obj_t * env) {
  (void)env;

  ae_obj_t * ret = ae_apply(CAR(list), env, CDR(list));
  
#ifdef AE_LOG_EVAL
  LOG(ret, "<= rtrn applied");
#endif

  return ret;
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

  MAYBE_EVAL_AND_BAIL_ON_ERROR(SPECIALP(fun), args);

  ae_obj_t * ret = NIL;
  
#ifdef AE_EVAL_EARLY_RETURN_ON_ERROR
  if (ERRORP(args))
    ret = args;
  else
#endif
    ret = (*CORE_FUN(fun))(env, args);
  
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
  LOG(FUN_PARAMS(fun), "appl user fun params");
  LOG(FUN_BODY(fun), "appl user fun body");
  LOG(args, "appl user fun args");
  LOG(fun, "applying user fun");
  LOG(env, "appl user fun env");
#endif
  
  ae_obj_t * new_env = NIL;

#ifndef AE_NO_SINGLE_SYM_PARAMS  
  if (SYMBOLP(FUN_PARAMS(fun))
      && (! NILP(FUN_PARAMS(fun)))
  )
    new_env = NEW_ENV(FUN_ENV(fun), CONS(FUN_PARAMS(fun), NIL), CONS(args, NIL));
  else
#endif
    new_env = NEW_ENV(FUN_ENV(fun), FUN_PARAMS(fun), args);

#ifdef AE_LOG_EVAL
  OLOG(new_env); NL;
#endif 
  
  ae_obj_t * body    = CONS(SYM("progn"), FUN_BODY(fun));
  
#ifdef AE_LOG_EVAL
  PR("\n\n[created exec env]");
  LOG(fun,              "exec env for");
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
    {AE_CHAR,     &self},
    {AE_CONS,     &apply},
    {AE_CORE,     &self},
    {AE_ERROR,    &self},
    {AE_ENV,      &self},
    {AE_FLOAT,    &self},
    {AE_INTEGER,  &self},
    {AE_LAMBDA,   &self},
    {AE_MACRO,    &self},
    {AE_RATIONAL, &self},
    {AE_STRING,   &self},
    {AE_SYMBOL,   &lookup},
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_eval(ae_obj_t * env, ae_obj_t * obj) {  
#ifdef AE_LOG_EVAL
  PR("\n\n[dispatching eval...]");
  LOG(obj, "disp eval for obj");
  LOG(env, "disp eval env");
#endif

  assert(ENVP(env));

  eval_dispatch_t dispatch = {0};
  
  GET_DISPATCH(dispatch, eval_dispatch, obj);

#ifdef AE_LOG_EVAL
  PR("\n=> dispatching eval to   %s handler for ", TYPE_STR(obj));
  WRITE(obj);
  DOT;
#endif

  assert(*dispatch.handler);
  
  return (*dispatch.handler)(obj, env);
  
#ifdef AE_LOG_EVAL
  fprintf(stderr, "\nDon't know how to eval a %s.", TYPE_STR(obj));
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
  LOG(fun,  "disp appl for fun");
  LOG(args, "disp appl args");
  LOG(env,  "disp appl env");
#endif

  fun = EVAL(env, fun);

#ifdef AE_EVAL_EARLY_RETURN_ON_ERROR
  if (ERRORP(fun))
    return fun;
#endif

#ifdef AE_LOG_EVAL
  LOG(fun, "apply fun");
#endif

  if (! (COREP(fun) || LAMBDAP(fun) || MACROP(fun))) {
    NL;
    PR("Not applicable: ");
    PUT(fun);
    NL;
    
    assert(0);
  }

  assert(TAILP(args));

  apply_dispatch_t dispatch = {0};
  
  GET_DISPATCH(dispatch, apply_dispatch, fun);

#ifdef AE_LOG_EVAL
  PR("\n=> dispatching apply to  %s handler for ", TYPE_STR(fun));
  WRITE(fun);
  DOT;
#endif

  MAYBE_EVAL_AND_BAIL_ON_ERROR(dispatch.special, args);

  ae_obj_t * ret = NIL;

#ifdef AE_EVAL_EARLY_RETURN_ON_ERROR
  if (ERRORP(args))
    ret = args;
  else
#endif
    ret = dispatch.special
    ? (*dispatch.handler)(fun, env, args)
    : (*dispatch.handler)(fun, env, args);

  return ret;
  
  fprintf(stderr, "\nDon't know how to apply a %s.", TYPE_STR(fun));
  assert(0);
}
