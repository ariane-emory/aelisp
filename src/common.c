#include <getopt.h>
#include <libgen.h>
#include <stdarg.h>
#include <string.h>
#include <mach-o/dyld.h>
#include <sys/syslimits.h>

#include "common.h"

#include "capture.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "plist.h"
#include "log.h"
#include "obj.h"
#include "write.h"

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// externs
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
int yyparse (void);
void yyrestart(FILE * input_file);
extern int yylineno;
extern FILE * yyin;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// data
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool       log_core                  = false;
bool       log_eval                  = false;
bool       log_macro                 = false;
bool       read_error                = false;
char       mem[free_list_size]       = { 0 };
ae_obj_t * filename_stack            = NIL;
ae_obj_t * line_stack                = NIL;
ae_obj_t * program                   = NIL;
const setopts_flag_t default_std_opt = STD_MONO;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// preface
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
void preface(void) {
  NL;
  size_t pool_size = sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE;
  
  printf("obj size:          %d.\n",    sizeof(ae_obj_t));
  printf("int size:          %d.\n",    sizeof(int));
  printf("nil is at:         %016p.\n", NIL);
  printf("t is at:           %016p.\n", TRUE);
  printf("Pool first:        %016p.\n", pool_first);
  printf("Pool last:         %016p.\n", pool_last);
  printf("Pool size:         %016p (%zu bytes / %zu kb / %zu mb / %zu gb).\n",
         pool_size, pool_size, pool_size >> 10, pool_size >> 20, pool_size >> 30);

  printf("Strings pool size: %016p (%zu bytes).", free_list_size, free_list_size);
  NL;
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// paint_parsed
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
void paint_parsed(void) {
#ifdef AE_PAINT_EARLY_OBJECTS
//==================================================================================================
// paint the objects read from file with origin = read
//==================================================================================================

  PR("Painting objects read from file with origin = read...");
  NL;
    
  for (int ix = 0; ix < AE_OBJ_POOL_SIZE; ix++)
    if (! FREEP(pool_get_object(ix)) && ! HAS_PROP("origin", pool_get_object(ix))) {

#ifdef AE_LOG_KVP_SET_GET
      LOG(pool_get_object(ix), "#%d: Setting origin to 'read'", ix);
#endif
      
      PROPS(pool_get_object(ix)) = read_origin;
    }

  PR("Done painting objects read from file with origin = read.");
  NL;  
#endif
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// setopts
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

int setopts(int argc, char *argv[]) {
  int result = 0;
  
  int opt;
  bool got_std_opt = false;

  while ((opt = getopt(argc, argv, "mel:s:")) != -1) {
    switch (opt) {
    case 'm':
      result |= MICROBENCH_ENABLED;
      break;
    case 'e':
      result |= EARLY_LOGGING;
      break;
    case 'l':
      for (int i = 0; optarg && optarg[i]; i++) {
        switch (optarg[i]) {
        case 'c':
          result |= LOG_CORE;
          break;
        case 'e':
          result |= LOG_EVAL;
          break;
        case 'm':
          result |= LOG_MACRO;
          break;
        default:
          goto fail;
        }
      }
      break;
    case 's':
      if (got_std_opt)
        goto fail;

      if (strcmp(optarg, "f") == 0) {
        result |= STD_FUNDAMENTAL;
        got_std_opt = true;
      /* } else if (strcmp(optarg, "s") == 0) { */
      /*   result |= SPLIT_STD; */
      /*   got_std_opt = true; */
      } else if (strcmp(optarg, "n") == 0) {
        result |= NO_STD;
        got_std_opt = true;
      } else if (strcmp(optarg, "m") == 0) {
        result |= STD_MONO;
        got_std_opt = true;
      } else {
        goto fail;
      }
      break;
    default:
      goto fail;
    }
  }

  if (! got_std_opt)
    result |= default_std_opt;
  
  result |= OPTS_OK;
  
  return result;

fail:
  // fprintf(stderr, "Usage: %s [-e] [-m] [-l c|e|m] [-s f|s|m|n]\n", argv[0]);
  fprintf(stderr, "Usage: %s [-e] [-m] [-l c|e|m] [-s f|m|n]\n", argv[0]);

  return result;
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * load_file(const char * filename, bool * const failed_to_open) {
  FILE * const original_yyin = yyin;
  yyin = fopen(filename, "r");

  program = NIL;

  if (!yyin) {
    PR("Failed to open file '%s'.\n", filename);

    if (failed_to_open != NULL)
      *failed_to_open = true;
    
    char * const err_msg_tmp = free_list_malloc(256);
    sprintf(err_msg_tmp, "Failed to open file '%s'.", filename);
    char * const err_msg     = free_list_malloc(strlen(err_msg_tmp) + 1);
    strcpy(err_msg, err_msg_tmp);
    free_list_free(err_msg_tmp);

    return NEW_ERROR(err_msg);
  }
  else if (failed_to_open != NULL) {
    *failed_to_open = false;
  }

  char * const file_basename     = basename((char *)filename);
  char * const file_basename_str = free_list_malloc(strlen(file_basename) + 1);
  
  strcpy(file_basename_str, file_basename);

  ae_obj_t * const  loaded_file  = NEW_STRING(file_basename_str);

  PUSH(loaded_file, filename_stack);   // current file 
  PUSH(NEW_INT(yylineno), line_stack); // line in previous file
  
  yylineno = 0;
  yyrestart(yyin);
  yyparse();
  
  fclose(yyin);

  yyin = original_yyin;

  POP(filename_stack);
  yylineno = INT_VAL(POP(line_stack));
  
  return program; 
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper macros we'll use to load the core functions:
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define COUNT_ARGUMENTS(...) COUNT_ARGUMENTS_HELPER(__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define COUNT_ARGUMENTS_HELPER(_1, _2, _3, _4, _5, _6, _7, _8, _9, N, ...) N
#define load_fun(c_name, special, min_args, max_args, ...)                                                                  \
  load_fun_helper(env, #c_name, &ae_core_##c_name, special, min_args, max_args, COUNT_ARGUMENTS(__VA_ARGS__), __VA_ARGS__);
#define add_core_op(name, oper, default, no_zero_args, sym)                                        \
  {                                                                                                \
    ae_obj_t * new_core = NEW_CORE(#name, &ae_core_##name, false, 1, 15);                          \
    ENV_SET(env, SYM(#oper), new_core);                                                            \
    ENV_SET(env, SYM(sym),  new_core);                                                             \
  }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// load_fun_helper
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// _new_root
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_common_new_root(bool log_loading_std, int flags) {
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 1: Stash the old values of the log flags, we'll restore them later:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  /* bool old_log_core  = log_core; */
  /* bool old_log_eval  = log_eval; */
  /* bool old_log_macro = log_macro; */

  /* log_core  = false; */
  /* log_eval  = false; */
  /* log_macro = false; */
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 2: Clear out the symbols_list, object pool and string pool/free list:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  symbols_list = NIL;
  pool_clear();
  free_list_reset();
  free_list_add_block(&mem[0], free_list_size);

  // Reset the properties of nil and t: this is is only really necessary if we're going to build
  // the root env more than once (such as is done by, for example, the unit tests, or the REPL's
  // reset command): since we just cleared the pool, whatever plist they referred to is gone, so
  // we fix the dangling pointer here.
  
  PROPS(NIL)  = NIL;
  PROPS(TRUE) = NIL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 3: Create the root environment:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  ae_obj_t * const env = NEW_ENV(NIL, NIL, NIL);
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 4: Load all the core functions and set a few aliases:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  FOR_EACH_CORE_FUN_GROUP_2(load_fun);
  FOR_EACH_CORE_FUN_GROUP_3(load_fun);
  FOR_EACH_CORE_MATH_OP(add_core_op);
  FOR_EACH_CORE_FUN_GROUP_1(load_fun);
  FOR_EACH_CORE_CMP_OP(add_core_op);

  bool equal_found = false;
  ae_obj_t * const equal = ENV_GET(env, SYM("=="), &equal_found);
  assert(equal_found); // If this fails, something has gone horribly wrong.
  ENV_SET(env, SYM("="), equal);
  
  FOR_EACH_CORE_FUN_GROUP_4(load_fun);
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 5: Do a little song and dance to put the home dir, lib dir and data dir into *load-path*:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 6: Set some system variables and mark some of them as constant and/or read-only:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Set *std-name* based on the std_mode passed from the command line:

  if      (flags & STD_FUNDAMENTAL)
    ENV_SET(env, SYM("*std-name*"), SYM("std-fundamental"));
  /* else if (flags & SPLIT_STD) */
  /*   ENV_SET(env, SYM("*std-name*"), SYM("std")); */
  else if (flags & NO_STD)
    ENV_SET(env, SYM("*std-name*"), NIL);
  else if (flags & STD_MONO)
    ENV_SET(env, SYM("*std-name*"), SYM("mono-std"));
  else
    assert(false && "Invalid std_mode");

  // This is constant because it's set by the command line on startup and chang it wouldn't do anything anyhow:
  PUT_PROP(TRUE, "constant",      SYM("*std-name*"));
  PUT_PROP(TRUE, "no-user-props", SYM("*std-name*"));
  
  // *features* should always be ENV_BOUNDP.
  ENV_SET(env,                SYM("*features*"),                    NIL); 

  // These two are constant because changing them wouldn't do anything until std is reloaded anyhow:
  ENV_SET(env,                     SYM("*log-loading-std-enabled*"),     TRUTH(log_loading_std));

  PUT_PROP(TRUE, "constant",       SYM("*log-loading-std-enabled*"));
  PUT_PROP(TRUE, "no-user-props",  SYM("*log-loading-std-enabled*"));

  ENV_SET(env,                     SYM("*microbench-enabled*"),          TRUTH(flags & MICROBENCH_ENABLED));

  PUT_PROP(TRUE, "constant",       SYM("*microbench-enable*"));
  PUT_PROP(TRUE, "no-user-props",  SYM("*microbench-enable*"));

  // *program* should always be bound but is constant, the user probably shouldn't change it anyhow:
  /* bool program_found = false; */
  /* ENV_SET(env,                     SYM("*program*"),                     NIL); */
  /* PUT_PROP(TRUE, "constant",       SYM("*program*")); */
  /* PUT_PROP(TRUE, "no-user-props",  SYM("*program*")); */

  // Of course, nil and t are constants:
  PUT_PROP(TRUE, "constant",            NIL);
  PUT_PROP(TRUE, "no-user-props",       NIL);

  PUT_PROP(TRUE, "constant",            TRUE);
  PUT_PROP(TRUE, "no-user-props",       TRUE);
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 6: Set some system variables and mark some of them as constant:
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  bool             std_name_found = false; 
  ae_obj_t * const std_name       = ENV_GET(env, SYM("*std-name*"), &std_name_found);

  if (! NILP(std_name)) {
    assert(std_name_found); // We just set this a page or so back, so if it's not found something is very broken.
    assert(std_name);
    assert(SYMBOLP(std_name));
    assert(! NILP(std_name)); // We just set it, so it better not be nil.
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Step 7: Finally, require whichever version of the stdlib was chosen:
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////

    const ae_obj_t * const std_return = ae_core_require(env, CONS(std_name, NIL), 1);

    if (ERRORP(std_return)) {
      FPR(stderr, "\nWARNING: Failed to load std: ");
      WRITE(std_return);
      NL;
    }
  }  

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Step 8: Restore the log flags.
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /* log_core  = old_log_core; */
  /* log_eval  = old_log_eval; */
  /* log_macro = old_log_macro; */
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // All done!
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////

  return env;
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
