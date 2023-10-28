#include <string.h>
#include <getopt.h>
#include <mach-o/dyld.h>
#include <libgen.h>
#include <sys/syslimits.h>

#include "common.h"

#include "capture.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "list.h"
#include "log.h"
#include "obj.h"
#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// extern data
////////////////////////////////////////////////////////////////////////////////////////////////////
int yyparse (void);
void yyrestart(FILE * input_file);
extern int yylineno;
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// data
////////////////////////////////////////////////////////////////////////////////////////////////////
bool       log_core            = false;
bool       log_eval            = false;
bool       log_macro           = false;
bool       log_loading_std     = false;;
bool       enable_microbench   = true;
std_mode_t std_mode            = MONO_STD;
bool       read_error          = false;
char       mem[free_list_size] = { 0 };
ae_obj_t * filename_stack      = NIL;
ae_obj_t * line_stack          = NIL;
ae_obj_t * program             = NIL;
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// preface
////////////////////////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// setup_root_env
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * setup_root_env(void) {
  bool old_log_core  = log_core;
  bool old_log_eval  = log_eval;
  bool old_log_macro = log_macro;

  log_core  = false;
  log_eval  = false;
  log_macro = false;
  
//==================================================================================================
// set up the free list and populate the root env
//==================================================================================================

  symbols_list = NIL;
  pool_clear();
  free_list_reset();
  free_list_add_block(&mem[0], free_list_size);

  ae_obj_t * const env            = ENV_NEW_ROOT;
  bool             std_name_found = false;
  ae_obj_t * const std_name       = ENV_GET(env, SYM("*std-name*"), &std_name_found);

  assert(std_name_found);
  assert(std_name);
  assert(NILP(std_name) || SYMBOLP(std_name));
  assert(! NILP(std_name)); // temporarily disallowed.
  
  if (! NILP(std_name)) {
    ae_obj_t const * std_return = ae_core_require(env, CONS(std_name, NIL), 1);

    NL;
    NL;
    
    if (ERRORP(std_return)) {
      FPR(stderr, "\nWARNING: Failed to load std: ");
      WRITE(std_return);
      NL;
    }
  }
  
  log_core  = old_log_core;
  log_eval  = old_log_eval;
  log_macro = old_log_macro;
  
  return env;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// paint_parsed
////////////////////////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// setopts
////////////////////////////////////////////////////////////////////////////////////////////////////
#include <string.h>  // include this for strcmp()

bool setopts(int argc, char *argv[]) {
  int opt;
  bool got_std_opt = false;

  while ((opt = getopt(argc, argv, "l:s:")) != -1) {
    switch (opt) {
    case 'l':
      for (int i = 0; optarg && optarg[i]; i++) {
        switch (optarg[i]) {
        case 'c':
          log_core = true;
          break;
        case 'e':
          log_eval = true;
          break;
        case 'm':
          log_macro = true;
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
        std_mode = STD_FUNDAMENTAL_ONLY;
        got_std_opt = true;
      } else if (strcmp(optarg, "s") == 0) {
        std_mode = SPLIT_STD;
        got_std_opt = true;
      } else if (strcmp(optarg, "m") == 0) {
        std_mode = MONO_STD;
        got_std_opt = true;
      } else {
        goto fail;
      }
      break;
    default:
      goto fail;
    }
  }

  return true;

fail:
  fprintf(stderr, "Usage: %s [-l c|e|m] [-s f|s|m]\n", argv[0]);
  return false;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
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

  ae_obj_t * loaded_file         = NEW_STRING(file_basename_str);

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
////////////////////////////////////////////////////////////////////////////////////////////////////
