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
#include "utility.h"
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
bool       no_std           = false;
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
  bool old_log_core = log_core;
  bool old_log_eval = log_eval;
  bool old_log_macro = log_macro;

  log_core = false;
  log_eval = false;
  log_macro = false;
  
#ifdef AE_PREFACE
  preface();
#endif
  
//==================================================================================================
// set up the free list and populate the root env
//==================================================================================================

  symbols_list = NIL;
  pool_clear();
  free_list_reset();
  free_list_add_block(&mem[0], free_list_size);

  ae_obj_t * root_env = ENV_NEW_ROOT();

  
////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_PAINT_EARLY_OBJECTS
////////////////////////////////////////////////////////////////////////////////////////////////////

//==================================================================================================
// set up the read_origin and promordial_origin objects
//==================================================================================================
 
  NL;
  
  ae_obj_t * primordial_origin = NIL;
  KSET(primordial_origin, KW("origin"), KW("primordial"));
  
  ae_obj_t * read_origin = NIL;
  KSET(read_origin, KW("origin"), KW("read"));

//==================================================================================================
// paint the objects populating the root env with origin = primordial
//==================================================================================================

  PR("Painting objects populating the root env with origin = primordial...");
  NL;
  
  for (int ix = 0; ix < AE_OBJ_POOL_SIZE; ix++)
    if (! FREEP(pool_get_object(ix))) {
#ifdef AE_LOG_KVP_SET_GET
      //LOG(pool_get_object(ix), "#%d: Setting origin to 'primordial'", ix);
#endif
      
      PROPS(pool_get_object(ix)) = primordial_origin;
    }

  PR("Done painting objects populating the root env with origin = primordial.");
  NL;
  
////////////////////////////////////////////////////////////////////////////////////////////////////
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////

  if (! no_std) {
    const char * const std_rel_path = "/../lib/std.lisp";
    char * const       std_path     = free_list_malloc(PATH_MAX);
    uint32_t           size            = PATH_MAX;
  
    if (_NSGetExecutablePath(std_path, &size) == 0) {
      char * const tmp = free_list_malloc(strlen(dirname(std_path))+1);

      strcpy(tmp, dirname(std_path));
      strcpy(std_path, tmp);
      strcat(std_path, std_rel_path);
    
      free_list_free(tmp);

      PR("Loading std from %s... ", std_path);
    } else {
      FPR(stderr, "Buffer too small, need %d bytes!\n", size);
    
      exit(1);
    }

    bool failed_to_load = false;
  
    ae_obj_t * const program  = load_file(std_path, &failed_to_load);

    free_list_free(std_path);

    if (failed_to_load)
      FPR(stderr, "WARNING: Failed to load std!\n");
    else
      PR("loaded.\n");

    ae_obj_t * const ret = EVAL(root_env, program);

    if (ERRORP(ret)) {
      FPR(stderr, "WARNING: Error evaluating std: ");
      WRITE(ret);
      putchar('!');
      NL;
    }
  }

  log_core = old_log_core;
  log_eval = old_log_eval;
  log_macro = old_log_macro;
  
  return root_env;
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
bool setopts(int argc, char *argv[]) {
  int opt;

  while ((opt = getopt(argc, argv, "l:n")) != -1) {
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
          fprintf(stderr, "Usage: %s [-lcem] [-n]\n", argv[0]);
          return false;
        }
      }
      break;
    case 'n':
      no_std = true;
      break;
    default:
      fprintf(stderr, "Usage: %s [-lcem] [-n]\n", argv[0]);
      return false;
    }
  }

  return true;
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

    return MAKE_ERROR(err_msg);
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
  
  PUT_PROP_RAW(program, loaded_file, SYM("*program*"));
  
  return program; 
}
////////////////////////////////////////////////////////////////////////////////////////////////////
