#include <stdarg.h>
#include <mach-o/dyld.h>
#include <libgen.h>
#include <sys/syslimits.h>

#include "env.h"

#include "core.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "log.h"

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

#ifdef AE_LOG_ENG
  LOG(value,   "with value");
  LOG(env,     "to env");
#endif
  
  // OUTDENT;

#ifdef AE_LOG_ENG
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
  assert(ENVP(env));

  if (! SYMBOLP(symbol))
    LOG(symbol, "NOT A SYMBOL");
  
  assert(SYMBOLP(symbol));
  assert(! KEYWORDP(symbol));
  assert(value);

  int local_indents = 0;
  
#ifdef AE_LOG_ENV
  LOG(symbol,    "[setting]");

  // INDENT;

  LOG(value,   "to value");
#endif

  ae_obj_t * pos     = env;

  // If GLOBAL, dive right to the top:
  if (mode == GLOBAL)
    while (! NILP(ENV_PARENT(pos)))
      pos = ENV_PARENT(pos);

  while (! NILP(pos)) { // loop through envs
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
        LOG(syms, "syms after");
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
  // OUTDENT;

  SLOG("[done setting]");
#endif
  
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


////////////////////////////////////////////////////////////////////////////////////////////////////
// load_fun_helper
////////////////////////////////////////////////////////////////////////////////////////////////////
typedef ae_obj_t * (*ae_core_fun_t)(ae_obj_t * const env, ae_obj_t * const args, int args_length);
//==================================================================================================
static void load_fun_helper(
  ae_obj_t   *  const env,
  const char *  const c_name,
  ae_core_fun_t const fun,
  bool                special,
  unsigned int        min_args,
  unsigned int        max_args,
  int                 count,
  ...) {
  va_list args;

  va_start(args, count);

  bool set_alt_name = false;

  ae_obj_t * const new_core = NEW_CORE(c_name, fun, special, min_args, max_args);
  
  for (int ix = 0; ix < count; ix++) {
    char * alt_name = va_arg(args, char *);

    if (alt_name == FUNDEF_END) 
      break;

    ENV_SET(env, SYM(alt_name), new_core);

    set_alt_name = true;
  }

  if (! set_alt_name)
    ENV_SET(env, SYM(c_name), new_core);
  
  va_end(args);
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper macros we'll use to load the core functions:
////////////////////////////////////////////////////////////////////////////////////////////////////
#define COUNT_ARGUMENTS(...) COUNT_ARGUMENTS_HELPER(__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define COUNT_ARGUMENTS_HELPER(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define load_fun(c_name, special, min_args, max_args, ...)                                                                  \
  load_fun_helper(env, #c_name, &ae_core_##c_name, special, min_args, max_args, COUNT_ARGUMENTS(__VA_ARGS__), __VA_ARGS__);
#define add_core_op(name, sym, ...)                                                                \
  {                                                                                                \
    ae_obj_t * new_core = NEW_CORE(#name, &ae_core_##name, false, 1, 15);                          \
    ENV_SET(env, SYM(#sym),  new_core);                                                            \
    ENV_SET(env, SYM(#name), new_core);                                                            \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _new_root: This function is huge, maybe it shoul be moved into common?
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_env_new_root(bool log_loading_std, bool enable_microbench, std_mode_t mode) {

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 1: Stash the old values of the log flags, we'll restore them later:
  //////////////////////////////////////////////////////////////////////////////////////////////////
  
  /* bool old_log_core  = log_core; */
  /* bool old_log_eval  = log_eval; */
  /* bool old_log_macro = log_macro; */

  /* log_core  = false; */
  /* log_eval  = false; */
  /* log_macro = false; */
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 2: Clear out the symbols_list, object pool and string pool/free list:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  symbols_list = NIL;
  pool_clear();
  free_list_reset();
  free_list_add_block(&mem[0], free_list_size);

  // Reset the properties of nil and t: this is is only really necessary if we're going to build
  // the root env more than once (such as is done by, for example, the unit tests): since we just
  // cleared the pool, whatever plist they referred to is gone.
  PROPS(NIL)  = NIL;
  PROPS(TRUE) = NIL;

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 3: Create the root environment:
  //////////////////////////////////////////////////////////////////////////////////////////////////
  
  ae_obj_t * const env = NEW_ENV(NIL, NIL, NIL);
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 4: Load all the core functions and set a few aliases:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  FOR_EACH_CORE_FUN_GROUP_2(load_fun);

  {
    bool found = false;
    
#if AE_PREFER_ALIST
    ENV_SET(env, SYM("khas?"), ENV_GET(env, SYM("ahas?"), &found));
    ENV_SET(env, SYM("kset"),  ENV_GET(env, SYM("aset"), &found));
    ENV_SET(env, SYM("kget"),  ENV_GET(env, SYM("aget"), &found));
#else
    ENV_SET(env, SYM("khas?"), ENV_GET(env, SYM("phas?"), &found));
    ENV_SET(env, SYM("kset"),  ENV_GET(env, SYM("pset"), &found));
    ENV_SET(env, SYM("kget"),  ENV_GET(env, SYM("pget"), &found));
#endif
  }
  
  FOR_EACH_CORE_FUN_GROUP_3(load_fun);
  FOR_EACH_CORE_MATH_OP(add_core_op);
  FOR_EACH_CORE_FUN_GROUP_1(load_fun);
  FOR_EACH_CORE_CMP_OP(add_core_op);

  bool equal_found = false;
  ae_obj_t * const equal = ENV_GET(env, SYM("=="), &equal_found);
  assert(equal_found); // If this fails, something has gone horribly wrong.
  ENV_SET(env, SYM("="), equal);
  
  FOR_EACH_CORE_FUN_GROUP_4(load_fun);
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 5: Do a little song and dance to put the home dir, lib dir and data dir into *load-path*:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  /* */ char *       home_path         = NULL;
  const char * const lib_dir_rel_path  = "lib";
  const char * const data_dir_rel_path = "data";
  
  char * const bin_path = free_list_malloc(PATH_MAX);
  uint32_t     size     = PATH_MAX;
  
  if (_NSGetExecutablePath(bin_path, &size) != 0) {
    FPR(stderr, "Buffer too small, need %d bytes!\n", size); 

    exit(1);
  }

  const char * const home_path_tmp = dirname(dirname(bin_path));    
  home_path = free_list_malloc(strlen(home_path_tmp) + 1);
  strcpy(home_path, home_path_tmp);
  free_list_free(bin_path);

  const int    lib_dir_len   = strlen(home_path) + 1 + strlen(lib_dir_rel_path)  + 1;
  const int    data_dir_len  = strlen(home_path) + 1 + strlen(data_dir_rel_path) + 1;
  char * const lib_dir_path  = free_list_malloc(lib_dir_len);
  char * const data_dir_path = free_list_malloc(data_dir_len);

  snprintf(lib_dir_path,  lib_dir_len,  "%s/%s", home_path, lib_dir_rel_path);
  snprintf(data_dir_path, data_dir_len, "%s/%s", home_path, data_dir_rel_path);

  ENV_PUSH(env, NEW_STRING(lib_dir_path),  SYM("*load-path*")); 
  ENV_PUSH(env, NEW_STRING(data_dir_path), SYM("*load-path*"));

  ae_obj_t * const home = NEW_STRING(home_path);
  ENV_PUSH(env, home,                      SYM("*load-path*"));
  ENV_SET(env,  SYM("*ae-home*"),          home);

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 6: Set some system variables and mark some of them as constant and/or read-only:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  // Set *std-name* based on the std_mode passed from the command line:
  switch (mode) {
  case STD_FUNDAMENTAL_ONLY:
    ENV_SET(env, SYM("*std-name*"), SYM("std-fundamental"));
    break;
  case SPLIT_STD:
    ENV_SET(env, SYM("*std-name*"), SYM("std"));
    break;
  case MONO_STD:
    ENV_SET(env, SYM("*std-name*"), SYM("mono-std"));
    break;
  default:
    assert(false); // This should be impossible.
  }

  // This is constant because it's set by the command line on startup and chang it wouldn't do anything anyhow:
  PUT_PROP(TRUE, "constant", SYM("*std-name*"));
  
  // *features* should always be ENV_BOUNDP.
  ENV_SET(env,                SYM("*features*"),                    NIL); 

  // These two are constant because changing them wouldn't do anything until std is reloaded anyhow:
  ENV_SET(env,                SYM("*log-loading-std-enabled*"),     TRUTH(log_loading_std));
  PUT_PROP(TRUE, "constant",  SYM("*log-loading-std-enabled*"));
  ENV_SET(env,                SYM("*microbench-enabled*"),          TRUTH(enable_microbench));
  PUT_PROP(TRUE, "constant",  SYM("*microbench-enable*"));

  // *program* should always be bound but is constant, the user probably shouldn't change it anyhow:
  bool program_found = false;
  ENV_SET(env,                SYM("*program*"),                     NIL);
  PUT_PROP(TRUE, "constant",  SYM("*program*"));

  // Of course, nil and t are constants:
  PUT_PROP(TRUE, "constant",  NIL);
  PUT_PROP(TRUE, "constant",  TRUE);
    
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 6: Set some system variables and mark some of them as constant:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  bool             std_name_found = false; 
  ae_obj_t * const std_name       = ENV_GET(env, SYM("*std-name*"), &std_name_found);

  assert(std_name_found); // We just set this a page or so back, so if it's not found something is very broken.
  assert(std_name);
  assert(SYMBOLP(std_name));
  assert(! NILP(std_name)); // We just set it, so it better not be nil.
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 7: Finally, load whichever version of the stdlib was chosen:
  //////////////////////////////////////////////////////////////////////////////////////////////////

  const ae_obj_t * const std_return = ae_core_require(env, CONS(std_name, NIL), 1);

  if (ERRORP(std_return)) {
    FPR(stderr, "\nWARNING: Failed to load std: ");
    WRITE(std_return);
    NL;
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 8: Restore the log flags.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  /* log_core  = old_log_core; */
  /* log_eval  = old_log_eval; */
  /* log_macro = old_log_macro; */
  
  //////////////////////////////////////////////////////////////////////////////////////////////////
  // All done!
  //////////////////////////////////////////////////////////////////////////////////////////////////

  return env;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
