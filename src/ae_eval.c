#include <stdio.h>

#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define NL            (putchar('\n'))
#define SPC           (putchar(' '))
#define PR(...)       (fprintf(stdout, __VA_ARGS__))
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

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
  PR("\n[Apply core fun]");
  NL;
  PR("fun           ");
  PUT(fun);
  SPC;
  PRINC(fun);
  NL;
  PR("args          ");
  PUT(args);
  SPC;
  PRINC(args);
  NL;  
  PR("env           ");
  PUT(env);
  SPC;
  PRINC(env);
  NL;
#endif

  ae_obj_t * ret = NIL;

  if (SPECIAL_FUNP(fun)) {
    // special funs get their un-evaluated args, plus the env.
    ae_obj_t * env_and_args = CONS(env, LIST(args)); 

    ret = (*FUN_VAL(fun))(env_and_args);
  }
  else {
    ae_obj_t * evaled_args = NIL;
    
    FOR_EACH(elem,  args)
      PUSH(evaled_args, EVAL(env, elem));

    ret = (*FUN_VAL(fun))(evaled_args);
  }
  
#ifdef AE_LOG_EVAL
  NL;
  PR("apply core fun returns ");
  PUT(ret);
  SPC;
  PRINC(ret);
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
  
  PR("fun           ");
  PUT(fun);
  SPC;
  PRINC(fun);
  NL;

  PR("params        ");
  PUT(fun->params);
  SPC;
  PRINC(fun->params);
  NL;

  PR("body          ");
  PUT(fun->body);
  SPC;
  PRINC(fun->body);
  NL;

  PR("env           ");
  PUT(env);
  SPC;
  PRINC(env);
  NL;

  PR("args          ");
  PUT(args);
  SPC;
  PRINC(args);
  NL;

  fflush(stdout);
#endif
  
  ae_obj_t * new_env = NEW_ENV(fun->env, fun->params, args);
  ae_obj_t * body    = CONS(INTERN("progn"), fun->body);

#ifdef AE_LOG_EVAL
  PR("\n[Created exec env]\n");
  PR("parent        ");
  PUT(new_env->parent);
  SPC;
  PRINC(new_env->parent);
  NL;
  PR("symbols       ");
  PUT(new_env->symbols);
  SPC;
  PRINC(new_env->symbols);
  NL;
  PR("values        ");
  PUT(new_env->values);
  SPC;
  PRINC(new_env->values);
  NL;
  PR("body          ");
  PUT(body);
  SPC;
  PRINC(body);
  NL;
#endif

  ae_obj_t * result = EVAL(new_env, body);

#ifdef AE_LOG_EVAL
  PR("result        ");
  PUT(result);
  SPC;
  PRINC(result);
  NL;
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
  { AE_CORE_FUN, &apply_core_fun },
  { AE_LAMBDA,   &apply_lambda   },
};

////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_apply(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args) {
#ifdef AE_LOG_EVAL
  PR("\n[Dispatch fun]\n");
  PR("fun            ");
  PUT(fun);
  SPC;
  PRINC(fun);
  NL;
  
  PR("args           ");
  PUT(args);
  SPC;
  PRINC(args);
  NL;
#endif

  fun = EVAL(env, fun); 

  ASSERT_FUNP(fun);
  ASSERT_TAILP(args);

  DISPATCH(apply_dispatch, fun, env, args);

  fprintf(stderr, "Don't know how to apply a %s.\n", TYPE_STR(GET_TYPE(fun)));
  assert(0);
}
