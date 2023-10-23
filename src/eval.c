#include <stdbool.h>
#include <stdio.h>

#include "eval.h"

// one or two of these includes might not by strictly necessary: 
#include "common.h"
#include "obj.h"
#include "free_list.h"
#include "log.h"
#include "env.h"
#include "jump_return.h"
#include "time_funcs.h"
#include "util.h"
#include "write.h"


////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define GET_DISPATCH(row, table, obj)                                                              \
  for (size_t ix = 0; ix < ARRAY_SIZE(table); ix++)                                                \
    if (table[ix].type == GET_TYPE(obj)) {                                                         \
      row = table[ix];                                                                             \
                                                                                                   \
      break;                                                                                       \
    }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EVAL_AND_RETURN_IF_ERRORP(env, o) RETURN_IF_ERRORP(EVAL((env), (o)))
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// snap_indent
////////////////////////////////////////////////////////////////////////////////////////////////////
static void snap_indent(void) {
  static int ctr = 0;
  // return;
  if (log_column > log_column_default) {
    if (++ctr > 4) {
      ctr = 0;
      log_column -= log_tab_width;
      if (log_column < log_column_default)
        log_column = log_column_default; // end of apply
    }
  }
  else {
    ctr = 0;
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////k


////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_eval_args
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_eval_args(ae_obj_t * const env, ae_obj_t * const args) {
  JUMP_RETURN_ENTER;

  assert(env);
  assert(ENVP(env));
  assert(args);
  assert(TAILP(args));
  
  const int args_count = LENGTH(args);
  
  if (log_eval)
    LOG(args, "evaluating fun's %d arg%s:", LENGTH(args), s_or_blank(args_count));

  RETURN_NIL_IF_NILP(args);
  
  INDENT;

  ae_obj_t * result_tail = NIL;
  ae_obj_t * current_arg = args;
  int        ctr         = 0;
  
  while (! ATOMP(current_arg)) {
    ++ctr;

    if (log_eval)
      LOG(CAR(current_arg), "eval   arg #%d/%d", ctr, args_count);

    INDENT;

    ae_obj_t* eval_result = EVAL_AND_RETURN_IF_ERRORP(env, CAR(current_arg));

    OUTDENT;

    if (log_eval)
      LOG(eval_result, "evaled arg #%d/%d", ctr, args_count);

    if (NILP(ret)) {
      ret = NEW_CONS(eval_result, NIL);
      result_tail = ret;
    } else {
      CDR(result_tail) = NEW_CONS(eval_result, NIL);
      result_tail = CDR(result_tail);
    }

    current_arg = CDR(current_arg);
  }

  if (! NILP(current_arg)) {
    if (log_eval)
      LOG(current_arg, "eval   tail arg");

    INDENT;

    ae_obj_t* eval_result = EVAL_AND_RETURN_IF_ERRORP(env, current_arg);

    // tail arg needs error handling!

    OUTDENT;

    if (log_eval)
      LOG(eval_result, "evaled tail arg");

    if (NILP(ret))
      ret = eval_result;
    else
      CDR(result_tail) = eval_result;

  }

  OUTDENT;
  
end:
  
  if (log_eval)
    LOG(ret, "evaluated fun's %d arg%s:", LENGTH(args), s_or_blank(LENGTH(args)));

  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply_core
////////////////////////////////////////////////////////////////////////////////////////////////////
static ae_obj_t * apply_core(ae_obj_t * env, ae_obj_t * fun, ae_obj_t * args) {
  JUMP_RETURN_ENTER;

  assert(env);
  assert(ENVP(env));
  assert(fun);
  assert(COREP(fun));
  assert(args);
  assert(TAILP(args));
  
  int args_length = LENGTH(args);
  
  if ((CORE_MIN_ARGS(fun) != 15 && LENGTH(args) < (int)CORE_MIN_ARGS(fun)) ||
      (CORE_MAX_ARGS(fun) != 15 && LENGTH(args) > (int)CORE_MAX_ARGS(fun))) {
    char * const err_msg_tmp = free_list_malloc(256);

    LOG(args, "invalid arg count:");
    
    // if CORE_MIN_ARGS(fun) == 15, then it has no minimum number of args, generate an appropriate message:
    if (CORE_MIN_ARGS(fun) == 15 && CORE_MAX_ARGS(fun) != 15)
      snprintf(err_msg_tmp, 256,
               "%s:%d: core '%s' requires at most %d args, but got %d",
               __FILE__,
               __LINE__,
               CORE_NAME(fun),
               CORE_MAX_ARGS(fun),
               LENGTH(args));
    else if (CORE_MAX_ARGS(fun) == 15 && CORE_MIN_ARGS(fun) != 15)
      snprintf(err_msg_tmp, 256,
               "%s:%d: core '%s' requires at least %d args, but got %d",
               __FILE__,
               __LINE__,
               CORE_NAME(fun),
               CORE_MIN_ARGS(fun),
               LENGTH(args));
    else if (CORE_MAX_ARGS(fun) == CORE_MIN_ARGS(fun))
      snprintf(err_msg_tmp, 256,
               "%s:%d: core '%s' requires %d arg%s, but got %d",
               __FILE__,
               __LINE__,
               CORE_NAME(fun),
               CORE_MIN_ARGS(fun),
               s_or_blank(CORE_MIN_ARGS(fun)),
               LENGTH(args));
    else
      snprintf(err_msg_tmp, 256,
               "%s:%d: core '%s' requires %d to %d args, but got %d",
               __FILE__,
               __LINE__,
               CORE_NAME(fun),
               CORE_MIN_ARGS(fun),
               CORE_MAX_ARGS(fun),
               LENGTH(args));

    char * const err_msg = free_list_malloc(strlen(err_msg_tmp) + 1);
    strcpy(err_msg, err_msg_tmp);
    free_list_free(err_msg_tmp);
    
    ae_obj_t * const err_data = NIL;

    KSET(err_data, KW("env"),  env);
    KSET(err_data, KW("args"), args);
    KSET(err_data, KW("fun"),  fun);

    ae_obj_t * err = NEW_ERROR(err_msg, err_data); 

    RETURN_IF_ERRORP(err);
  }

  {
    char * msg = NULL;

    if (log_eval)
      msg = free_list_malloc(256);
  
    if (! SPECIALP(fun)) {
      args = RETURN_IF_ERRORP(EVAL_ARGS(env, args));

      if (log_eval) 
        snprintf(msg, 256,
                 "applying core fun '%s' to %d evaled arg%s:",
                 CORE_NAME(fun), LENGTH(args), s_or_blank(LENGTH(args)));
    }
    else if (log_eval) {
      snprintf(msg, 256,
               "applying core fun '%s' to %d unevaled arg%s:",
               CORE_NAME(fun), LENGTH(args), s_or_blank(LENGTH(args)));
    }

    if (log_eval) {
      LOG(args, msg);
      
      free_list_free(msg);
    }
  }

  // this call might change the value of log_eval or log_core:
  RETURN((*CORE_FUN(fun))(env, args, args_length));

end:

  if (log_eval) {
    char * const msg = free_list_malloc(256);

    snprintf(msg, 256, 
             "applying core fun '%s' returned %s :%s",
             CORE_NAME(fun), a_or_an(GET_TYPE_STR(ret)), GET_TYPE_STR(ret));

    LOG(ret, msg);

    free_list_free(msg);
  }
  
  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// apply lambda fun
////////////////////////////////////////////////////////////////////////////////////////////////////
static ae_obj_t * apply_user(ae_obj_t * env, ae_obj_t * fun, ae_obj_t * args) {
  JUMP_RETURN_ENTER;
  
  assert(env);
  assert(ENVP(env));
  assert(fun);
  assert(LAMBDAP(fun) || MACROP(fun));
  assert(args);
  assert(TAILP(args));

  if (CONSP(FUN_PARAMS(fun)) &&
      ((LENGTH(args) < LENGTH(FUN_PARAMS(fun))) ||
       (PROPERP(FUN_PARAMS(fun)) && LENGTH(args) > LENGTH(FUN_PARAMS(fun))))
  ) {
    char * const msg_tmp = free_list_malloc(256);
    char * const fun_desc = SWRITE(fun);
    
    snprintf(msg_tmp,
             256,
             "%s:%d: user fun '%s' requires %s %d arg%s, but got %d",
             __FILE__,
             __LINE__,
             fun_desc,
             ! PROPERP(FUN_PARAMS(fun)) ? "at least" : "exactly",
             LENGTH(FUN_PARAMS(fun)),
             s_or_blank(LENGTH(FUN_PARAMS(fun))),
             LENGTH(args));

    free(fun_desc);
    
    char * msg = free_list_malloc(strlen(msg_tmp) + 1);
    strcpy(msg, msg_tmp);
    free_list_free(msg_tmp);

    ae_obj_t * err_data = NIL;

    KSET(err_data, KW("env"),  env);
    KSET(err_data, KW("args"), args);
    KSET(err_data, KW("fun"),  fun);

    RETURN_IF_ERRORP(NEW_ERROR(msg, err_data));
  }
    
  if (! SPECIALP(fun)) {
    args = RETURN_IF_ERRORP(EVAL_ARGS(env, args));
    
    if (log_eval)
      LOG(args, "applying user fun to %d evaled arg%s:", LENGTH(args), s_or_blank(LENGTH(args)));
  }
  else if (log_eval) {
    LOG(args, "applying user fun to %d unevaled arg%s:", LENGTH(args), s_or_blank(LENGTH(args)));
  }
  
//  ae_obj_t * body = FUN_BODY(fun);

  env = NEW_ENV(FUN_ENV(fun), FUN_PARAMS(fun), args);

  if (log_eval) {
    LOG(env,           "new env for user fun:");
    LOG(ENV_SYMS(env), "new env's syms:");
    LOG(ENV_VALS(env), "new env's vals:");
  }
  
  if (log_eval) {
    if (HAS_PROP("last-bound-to", fun)) {
      LOG(ENV_VALS(env), "applying user fun '%s' to %d arg%s",
          SYM_VAL(GET_PROP("last-bound-to", fun)), LENGTH(ENV_VALS(env)), s_or_blank(LENGTH(ENV_VALS(env))));
    }
    else {
      char * tmp = SWRITE(fun);
      LOG(ENV_VALS(env), "applying user fun %s to %d arg%s",
          tmp, LENGTH(ENV_VALS(env)), s_or_blank(LENGTH(ENV_VALS(env))));
      free(tmp);
    }
  }
  
  INDENT;

  ae_obj_t * body = FUN_BODY(fun);

  if (log_eval) {  
    // If FUN_PARAMS(fun) is a blob, we lie to get a plural length:
    LOG(FUN_PARAMS(fun), "as param%s", s_or_blank(CONSP(FUN_PARAMS(fun)) ? LENGTH(FUN_PARAMS(fun)) : 2));
    LOG(body,            "with body");
  }

  PUT_PROP(fun, "fun", env);

  ret = EVAL_AND_RETURN_IF_ERRORP(env, body);

  OUTDENT;

end:

  if (log_eval) {
    if (HAS_PROP("last-bound-to", fun)) {
      LOG(ret, "applying user fun '%s' returned %s :%s",
          SYM_VAL(GET_PROP("last-bound-to", fun)), a_or_an(GET_TYPE_STR(ret)), GET_TYPE_STR(ret));
    }
    else {
      char * tmp = SWRITE(fun);
      LOG(ret, "applying user fun %s returned %s :%s", tmp, a_or_an(GET_TYPE_STR(ret)), GET_TYPE_STR(ret));
      free(tmp);
    }
  }  

  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _apply
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * apply(ae_obj_t * env, ae_obj_t * obj) {
  JUMP_RETURN_ENTER;

  assert(env);
  assert(ENVP(env));
  assert(obj);
  assert(CONSP(obj));

  ae_obj_t * head = CAR(obj);
  ae_obj_t * args = CDR(obj);

  assert(args);
  assert(TAILP(args));

  if (log_eval) {
    char * tmp = SWRITE(head);
    char * msg = free_list_malloc(256);

    snprintf(msg, 256,
             "applying '%s' to %d arg%s:",
             tmp, LENGTH(args), s_or_blank(LENGTH(args)));
    
    LOG(args, msg);
    
    free(tmp);
    free_list_free(msg);
  }

  INDENT;

  if (log_eval) {
    LOG(env,  "in env");

    if (! ROOTP(env)) {
      LOG(ENV_SYMS(env), "with syms");
      LOG(ENV_VALS(env), "and  vals");
    }
  }

  ae_obj_t * fun = EVAL_AND_RETURN_IF_ERRORP(env, head);

  if (! (COREP(fun) || LAMBDAP(fun) || MACROP(fun))) {
    NL;
    LOG(head, "Result of evaluating head: ");
    LOG(fun,  "is inapplicable object: ");
    SLOGF("of type: %s", GET_TYPE_STR(fun));
    NL;

    ae_obj_t * const err_data = NIL;

    KSET(err_data, KW("env"),  env);
    KSET(err_data, KW("args"), args);
    KSET(err_data, KW("fun"),  fun);

    ae_obj_t * const err = NEW_ERROR("inapplicable", err_data);
    
    RETURN_IF_ERRORP(err);
  }

  long int begin = -1;
  
  if (MACROP(fun) && (log_eval || log_macro)) {
    LOG(obj, "expanding:");

    begin = now_us();
      
    INDENT;
  }

  ret = COREP(fun)
    ? apply_core(env, fun, args)
    : apply_user(env, fun, args);

  if (ERRORP(ret))
    goto end;

  if (MACROP(fun)) {
    if (log_eval || log_macro) {
      LOG(ret, "expansion:");
    }

    if (ATOMP(ret)) {
      ret = CONS(SYM("progn"), CONS(ret, NIL));

      if (log_eval || log_macro)
        LOG(ret, "decorated expansion:");
    }
    
    ret = EVAL(env, ret);

    long long int after = now_us();

    if (log_eval || log_macro)
      LOG(ret, "evaled expansion:");
    
    if (log_eval || log_macro) // inside if MACROP
      OUTDENT;

    if (log_eval || log_macro)
      LOG(obj, "expanding took %.2f ms:", ((double)(after - begin))/1000.0);

    if (ERRORP(ret))
      goto end;

    // this line would cause 'in-place expansion' and is disabled until a way
    // to annotate which macros should be expanded in-place is implemented:
    // *obj = *ret; 
  }

  if (ERRORP(ret)) {
    char * fun_tmp = SWRITE(fun);
    char * ret_tmp = SWRITE(ret);

    FSLOGF(stderr, "%s returned an error: %s", fun_tmp, ret_tmp);

    free(fun_tmp);
    free(ret_tmp);
    
    if (EHAS(ret, "fun")) // this is probably going to double the first fun in the list but I can't be bothered fixing it yet.
      ESET(ret, "fun", CONS(fun, EGET(ret, "fun")));
    else
      ESET(ret, "fun", CONS(fun, NIL));

    goto end;
  }

end:

  OUTDENT;

  if (log_eval)
    LOG(ret, "evaluating list returned %s :%s", a_or_an(GET_TYPE_STR(ret)), GET_TYPE_STR(ret));
  
  snap_indent();
  
  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// other _eval dispatch handlers
////////////////////////////////////////////////////////////////////////////////////////////////////
static ae_obj_t * self(ae_obj_t * env, ae_obj_t * obj) {
  assert(env);
  assert(ENVP(env));
  assert(obj);

  ae_obj_t * const ret = obj;
  
  if (log_eval)
    LOG(obj, "self-evaluated %s :%s",
        a_or_an(GET_TYPE_STR(obj)), GET_TYPE_STR(obj));

  return ret;
}
//==================================================================================================
static ae_obj_t * lookup(ae_obj_t * env, ae_obj_t * sym) {
  JUMP_RETURN_ENTER;

  assert(env);
  assert(ENVP(env));
  assert(sym);
  assert(SYMBOLP(sym));

  if (! ENV_BOUNDP(env, sym)) {
    ae_obj_t * err_data = NIL;
    KSET(err_data, KW("env"), env);
    KSET(err_data, KW("unbound-symbol"), sym);

    char * tmp = free_list_malloc(256);
    snprintf(tmp, 256,
             "%s:%d: unbound symbol '%s'",
             __FILE__, __LINE__, SYM_VAL(sym));
    char * msg = free_list_malloc(strlen(tmp) + 1);
    strcpy(msg, tmp);
    
    RETURN(NEW_ERROR(msg, err_data));
  }

  ret = KEYWORDP(sym)
    ? sym
    : ENV_FIND(env, sym);

end:

  if (log_eval)
    LOG(ret, "looked up '%s' and found %s :%s", SYM_VAL(sym), a_or_an(GET_TYPE_STR(ret)), GET_TYPE_STR(ret));

  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval dispatch table
////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct {
  ae_type_t type;
  ae_obj_t * (*handler)(ae_obj_t *, ae_obj_t *);
} eval_dispatch_row_t;
//==================================================================================================
static const eval_dispatch_row_t eval_dispatch_table[] = {
  { AE_CONS,     &apply,  },
  { AE_SYMBOL,   &lookup, },
  { AE_INTEGER,  &self,   },
  { AE_CORE,     &self,   },
  { AE_LAMBDA,   &self,   },
  { AE_MACRO,    &self,   },
  { AE_STRING,   &self,   },
  { AE_ENV,      &self,   },
  { AE_ERROR,    &self,   },
  { AE_CHAR,     &self,   },
  { AE_FLOAT,    &self,   },
  { AE_RATIONAL, &self,   },
};
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _eval
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_eval(ae_obj_t * const env, ae_obj_t * const obj) {
  JUMP_RETURN_ENTER;

  assert(env);
  assert(obj);
  assert(ENVP(env));

  RETURN_NIL_IF_NILP(obj);
  
  eval_dispatch_row_t dispatch = { 0 };

  GET_DISPATCH(dispatch, eval_dispatch_table, obj);

  assert(*dispatch.handler);

  RETURN((*dispatch.handler)(env, obj));

end:  

  JUMP_RETURN_EXIT;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
