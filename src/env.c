#include <mach-o/dyld.h>
#include <libgen.h>
#include <sys/syslimits.h>

#include "env.h"

#include "core.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "log.h"
#include "plist.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _add
////////////////////////////////////////////////////////////////////////////////////////////////////
void ae_env_add(ae_obj_t * const env, ae_obj_t * const symbol, ae_obj_t * const value) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));
  assert(! KEYWORDP(symbol));
  assert(value);

  int local_indents = 0;
  
  ENV_SYMS(env) = CONS(symbol, ENV_SYMS(env));
  ENV_VALS(env) = CONS(value,  ENV_VALS(env));

#ifdef AE_LOG_ENV
  LOG(symbol,  "[adding]");
#endif
  
  // INDENT;

#ifdef AE_LOG_ENV
  LOG(value,   "with value");
  LOG(env,     "to env");
  LOG(ENV_SYMS(env), "containing syms");
#endif
  
  // OUTDENT;

#ifdef AE_LOG_ENV
  LOG(symbol,  "[done adding]");
#endif
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _find
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_lookup(ae_env_lookup_mode_t mode,
                         ae_obj_t * const env,
                         const ae_obj_t *
                         const symbol,
                         bool * const found_ptr) {
  assert(ENVP(env));
  assert(SYMBOLP(symbol));

  int local_indents = 0;

#ifdef AE_LOG_ENV
  LOG(symbol, "[looking up '%s']", SYM_VAL(symbol));
#endif

  // INDENT;

  if (found_ptr)
    *found_ptr = false;

  // Check for keywords that are automatically resolved:
  if (KEYWORDP(symbol)) {

#ifdef AE_LOG_ENV
    LOG(NIL, "Keyword found automatically.");
#endif

    if (found_ptr)
      *found_ptr = true;

    // OUTDENT;
    
#ifdef AE_LOG_ENV
    LOG(symbol, "[looked up]");
#endif
    
    return (ae_obj_t *)symbol;
  }

  if (NILP(symbol)) {
    
#ifdef AE_LOG_ENV
    LOG(NIL, "found NIL automatically.");
#endif
    
    if (found_ptr)
      *found_ptr = true;
    
    // OUTDENT;
    
#ifdef AE_LOG_ENV
    LOG(NIL, "[looked up]");
#endif
    
    return NIL;
  }

  if (TRUEP(symbol)) {
    
#ifdef AE_LOG_ENV
    LOG(TRUE, "found TRUE automatically");
#endif
    
    if (found_ptr)
      *found_ptr = true;
    
    // OUTDENT;
    
#ifdef AE_LOG_ENV
    LOG(TRUE, "[looked up]");
#endif

    return TRUE;
  }

  ae_obj_t *ret = NIL;  // Initialize the return value
  ae_obj_t *pos = env;

  // If GLOBAL, dive right to the top:
  if (mode == GLOBAL)
    while (! NILP(ENV_PARENT(pos)))
      pos = ENV_PARENT(pos);

  for (; ENVP(pos); pos = ENV_PARENT(pos)) {
#ifdef AE_LOG_ENV
    LOG(pos, "in env");
#endif

    ae_obj_t *symbols = ENV_SYMS(pos);
    ae_obj_t *values  = ENV_VALS(pos);

#ifdef AE_LOG_ENV
    LOG(symbols, "containing syms");
#endif

    for (; CONSP(symbols); symbols = CDR(symbols), values = CDR(values)) {
      if (symbol == CAR(symbols)) {

#ifdef AE_LOG_ENV
        LOG(CAR(values), "found it ->");
#endif

        ret = CAR(values);

        if (found_ptr)
          *found_ptr = true;

        goto end;
      }
    }

    // Special case for symbols being one symbol:
    if (symbol == symbols) {
      ret = values;

      if (found_ptr)
        *found_ptr = true;

      goto end;
    }

    if (mode == LOCAL)
      break;
  }

#ifdef AE_LOG_ENV
  SLOG("didn't find it!");
#endif

end:
  // OUTDENT;
  
#ifdef AE_LOG_ENV
  LOG(ret, "[looked up]");
#endif

  return ret;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _set
////////////////////////////////////////////////////////////////////////////////////////////////////
void ae_env_set(
  ae_env_lookup_mode_t mode,
  ae_obj_t * const env,
  ae_obj_t * const symbol,
  ae_obj_t * const value) {

  assert(env);
  assert(ENVP(env));
  assert(symbol);
  if (! SYMBOLP(symbol)) {
    LOG(symbol, "NOT A SYMBOL!");
    NL;
  }
  assert(SYMBOLP(symbol));
  assert(! KEYWORDP(symbol));
  assert(value);

  ae_obj_t * pos = env;

  // If GLOBAL, dive right to the top:
  if (mode == GLOBAL)
    while (! NILP(ENV_PARENT(pos)))
      pos = ENV_PARENT(pos);

  while (! NILP(pos)) {
    ae_obj_t * syms = ENV_SYMS(pos);
    ae_obj_t * vals = ENV_VALS(pos);

    // Loop through proper or improper list
    while (CONSP(syms) && !NILP(vals)) {
      if (symbol == CAR(syms)) {
#ifdef AE_LOG_ENV
        LOG(value, "setting value ->");
#endif
        CAR(vals) = value;
        return;
      }
      syms = CDR(syms);
      vals = CDR(vals);
    }

    // Special case for symbols being one symbol:
    if (symbol == syms) {
#ifdef AE_LOG_ENV
      LOG(value, "setting value (single symbol) ->");
#endif
      ENV_VALS(pos) = value;
      return;
    }

    // If the symbol wasn't found and mode is LOCAL or we're at the topmost environment
    if (mode == LOCAL || NILP(ENV_PARENT(pos))) {
#ifdef AE_LOG_ENV
      LOG(value, "adding to environment ->");
#endif
      ENV_ADD(pos, symbol, value);
      return;
    } 
    else {
      pos = ENV_PARENT(pos);
    }
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _rootp
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_env_rootp(const ae_obj_t * const env) {
  assert(ENVP(env));

  return NILP(ENV_PARENT(env));
}
////////////////////////////////////////////////////////////////////////////////////////////////////

