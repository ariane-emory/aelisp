#include "env.h"
#include "eval.h"
#include "core.h"
#include "util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _add
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_add(ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));
  assert(! KEYWORDP(symbol));
  assert(value);
    
  ENV_SYMS(env) = CONS(symbol, ENV_SYMS(env));
  ENV_VALS(env) = CONS(value,  ENV_VALS(env));

#ifdef AE_LOG_ENV
  LOG(symbol,  "[adding]");

  INDENT;

  LOG(value,   "with value");
  LOG(env,     "to env");

  OUTDENT;

  LOG(symbol,  "[done adding]");
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_lookup(ae_env_set_mode_t mode, const ae_obj_t * const env, const ae_obj_t * const symbol, bool * const found) {

  assert(ENVP(env));
  assert(SYMBOLP(symbol));

#ifdef AE_LOG_ENV
  LOG(symbol, "[looking up]");
  
  INDENT;
#endif

  ae_obj_t * ret = NIL;

  if (found)
    *found = false;
  
  if (KEYWORDP(symbol)) {

#ifdef AE_LOG_ENV
    LOG(NIL, "Keyword found automatically.");
#endif

    ret = (ae_obj_t *)symbol;

    if (found)
      *found = true;

    goto end;
  }

  if (NILP(symbol)) {

#ifdef AE_LOG_ENV
    LOG(NIL, "found NIL automatically.");
#endif

    ret = NIL;

    if (found)
      *found = true;

    goto end;
  }

  if (TRUEP(symbol)) {

#ifdef AE_LOG_ENV
    LOG(TRUE, "found TRUE automatically");
#endif
    
    ret = TRUE;

    if (found)
      *found = true;

    goto end;
  }
  
  const ae_obj_t * pos = env;

  // If GLOBAL, let's just dive right to the top:
  if (mode == GLOBAL)
    while (! NILP(ENV_PARENT(pos)))
      pos = ENV_PARENT(pos);
  
  for (; ENVP(pos); pos = ENV_PARENT(pos)) { // loop through envs

#ifdef AE_LOG_ENV
    LOG(pos, "in env");
#endif
    
    ae_obj_t * symbols = ENV_SYMS(pos);
    ae_obj_t * values  = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    LOG(symbols, "containing syms");
#endif

    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values)) { // loop through syms/vals
      if (symbol ==  CAR(symbols)) {

#ifdef AE_LOG_ENV
        LOG(CAR(values), "found it ->"); 
#endif
        
        ret = CAR(values);

        if (found)
          *found = true;
        
        goto end;
      }
    } // end loop through syms/vals

    // special case for symbols being one symbol:
    if (symbol == symbols) {
      ret = values;

      goto end;
    }

    if (mode == LOCAL)
      break;
  } // end loop through envs
  
#ifdef AE_LOG_ENV
  SLOG("didn't find it!");
#endif
  
end:
  
#ifdef AE_LOG_ENV
  OUTDENT;

  LOG(ret, "[looked up]");
#endif
  
  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////

void ae_env_set(
  ae_env_set_mode_t mode,
  ae_obj_t * const env,
  ae_obj_t * const symbol,
  ae_obj_t * const value) {
  assert(ENVP(env));

  if (! SYMBOLP(symbol))
    LOG(symbol, "NOT A SYMBOL");
  
  assert(SYMBOLP(symbol));
  assert(! KEYWORDP(symbol));
  assert(value);
  
#ifdef AE_LOG_ENV
  LOG(symbol,    "[setting]");

  INDENT;

  LOG(value,   "to value");
#endif

  ae_obj_t * pos     = env;

  while (! NILP(pos)) { // loop through envs
    if (mode == GLOBAL && (! NILP(ENV_PARENT(pos))))
      goto go_up;
        
    ae_obj_t * syms = ENV_SYMS(pos);
    ae_obj_t * vals = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    LOG(pos,  "in env");
    LOG(syms, "containing syms");
#endif

    while (! NILP(syms) && ! NILP(vals)) { // loop through syms/vals
      ae_obj_t * sym = CAR(syms);

      if (symbol == sym) {

#ifdef AE_LOG_ENV
        LOG(syms, "found it in ->");
#endif

        CAR(vals) = value;

#ifdef AE_LOG_ENV
        LOG(vals, "values after");
#endif

        goto end;
      }

      syms = CDR(syms);
      vals = CDR(vals);
    } // end loop through syms/vals

    if (mode == LOCAL || NILP(ENV_PARENT(pos))) {
      ENV_ADD(pos, symbol, value);

      goto end;
    } else {

    go_up:
#ifdef AE_LOG_ENV
      SLOG("going up");
#endif
      
      pos = ENV_PARENT(pos);
    }
  } // end loop through envs

end:
  
#ifdef AE_LOG_ENV
  OUTDENT;

  SLOG("[done setting]");
#endif
  
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _new_root
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_env_new_root(void) {
  ae_obj_t * env = NEW_ENV(NIL, NIL, NIL);

#define add_core_fun(name, ...)          ENV_SET(env, SYM(#name), NEW_CORE(#name, &ae_core_##name, false));  
#define add_core_special_fun(name, ...)  ENV_SET(env, SYM(#name), NEW_CORE(#name, &ae_core_##name, true));
#define add_core_op(name, sym, ...)      ENV_SET(env, SYM(#sym),  NEW_CORE(#name, &ae_core_##name, false));
  
  FOR_EACH_CORE_MATH_OP(add_core_op);
  FOR_EACH_CORE_CMP_OP(add_core_op);
  FOR_EACH_SPECIAL_CORE_FUN(add_core_special_fun);
  FOR_EACH_CORE_FUN(add_core_fun);

#ifdef PREFER_ALIST
  ENV_SET(env, SYM("khas"), ENV_FIND(env, SYM("ahas")));
  ENV_SET(env, SYM("kget"), ENV_FIND(env, SYM("aget")));
  ENV_SET(env, SYM("kset"), ENV_FIND(env, SYM("aset")));
#else
  ENV_SET(env, SYM("khas"), ENV_FIND(env, SYM("phas")));
  ENV_SET(env, SYM("kget"), ENV_FIND(env, SYM("pget")));
  ENV_SET(env, SYM("kset"), ENV_FIND(env, SYM("pset")));
#endif

  ENV_SET(env, SYM("let*"), ENV_FIND(env, SYM("let_star")));
  ENV_SET(env, SYM("∨*"),   ENV_FIND(env, SYM("or")));
  ENV_SET(env, SYM("λ"),    ENV_FIND(env, SYM("lambda")));
  
  
  return env;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _rootp
////////////////////////////////////////////////////////////////////////////////////////////////////

bool ae_env_rootp(const ae_obj_t * const env) {
  assert(ENVP(env));

  return NILP(ENV_PARENT(env));
}
