#include <string.h>
#include <getopt.h>
#include <mach-o/dyld.h>
#include <libgen.h>
#include <sys/syslimits.h>

#include "common.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "obj.h"
#include "util.h"
#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// data
////////////////////////////////////////////////////////////////////////////////////////////////////

#define free_list_size (1 << 16)
char mem[free_list_size] = { 0 };

ae_obj_t * last_loaded_file = NIL;

ae_obj_t * program = NIL;

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

ae_obj_t * setup_root_env(void) {

#ifdef AE_PREFACE
  preface();
#endif
  
////////////////////////////////////////////////////////////////////////////////////////////////////
// set up the free list and populate the root env
////////////////////////////////////////////////////////////////////////////////////////////////////

  symbols_list = NIL;
  pool_clear();
  free_list_reset();
  free_list_add_block(&mem[0], free_list_size);

  ae_obj_t * root_env = ENV_NEW_ROOT();

  
////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_PAINT_EARLY_OBJECTS
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// set up the read_origin and promordial_origin objects
////////////////////////////////////////////////////////////////////////////////////////////////////
 
  NL;
  
  ae_obj_t * primordial_origin = NIL;
  KSET(primordial_origin, KW("origin"), KW("primordial"));
  
  ae_obj_t * read_origin = NIL;
  KSET(read_origin, KW("origin"), KW("read"));

////////////////////////////////////////////////////////////////////////////////////////////////////
// paint the objects populating the root env with origin = primordial
////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef AE_NO_STDLIB
////////////////////////////////////////////////////////////////////////////////////////////////////

  const char * const stdlib_rel_path = "/../lib/stdlib.lisp";
  char * const       stdlib_path     = free_list_malloc(PATH_MAX);
  uint32_t           size            = PATH_MAX;
  
  if (_NSGetExecutablePath(stdlib_path, &size) == 0) {
    char * const tmp = free_list_malloc(strlen(dirname(stdlib_path))+1);

    strcpy(tmp, dirname(stdlib_path));
    strcpy(stdlib_path, tmp);
    strcat(stdlib_path, stdlib_rel_path);
    
    free_list_free(tmp);

    PR("Loading stdlib from %s... ", stdlib_path);
  } else {
    FPR(stderr, "Buffer too small, need %d bytes!\n", size);
    
    exit(1);
  }

  bool failed_to_load = false;
  
  ae_obj_t * program = load_file(stdlib_path, &failed_to_load);

  free_list_free(stdlib_path);

  if (failed_to_load)
    FPR(stderr, "WARNING: Failed to load stdlib!\n");
  else
    PR("loaded.\n");

  ae_obj_t * ret     = EVAL(root_env, program);

  if (ERRORP(ret)) 
    FPR(stderr, "WARNING: Error evaluating stdlib!\n");  
  
////////////////////////////////////////////////////////////////////////////////////////////////////
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////
  
  return root_env;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void paint_parsed(void) {

////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_PAINT_EARLY_OBJECTS
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// paint the objects read from file with origin = read
////////////////////////////////////////////////////////////////////////////////////////////////////

  // pool_print();

  PR("Painting objects read from file with origin = read...");
  NL;
    
  for (int ix = 0; ix < AE_OBJ_POOL_SIZE; ix++)
    if (! FREEP(pool_get_object(ix)) && ! HAS_PROP(pool_get_object(ix), "origin")) {
#ifdef AE_LOG_KVP_SET_GET
      //  LOG(pool_get_object(ix), "#%d: Setting origin to 'read'", ix);
#endif
      
      PROPS(pool_get_object(ix)) = read_origin;
    }

  PR("Done painting objects read from file with origin = read.");
  NL;
  
////////////////////////////////////////////////////////////////////////////////////////////////////
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////
  
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// setopts
////////////////////////////////////////////////////////////////////////////////////////////////////

extern bool log_core;
extern bool log_eval;

bool setopts(int argc, char *argv[]) {
  int opt;

  while ((opt = getopt(argc, argv, "l:")) != -1) {
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
          fprintf(stderr, "Usage: %s [-lc] [-le]\n", argv[0]);
          return false;
        }
      }
      break;
    default:
      fprintf(stderr, "Usage: %s [-lc] [-le]\n", argv[0]);
      return false;
    }
  }

  // // For demonstration purposes
  // printf("log_core: %s\n", log_core ? "true" : "false");
  // printf("log_eval: %s\n", log_eval ? "true" : "false");

  return true;
}

////////////////////////////////////////////////////////////////////////////////

int yyparse (void);
void yyrestart(FILE * input_file);
extern int yylineno;

ae_obj_t * load_file(const char * filename, bool * const failed_to_open) {
  FILE * const original_yyin = yyin;
  yyin = fopen(filename, "r");

  program = NIL;

  if (!yyin) {
    PR("Failed to open file '%s'.\n", filename);

    if (failed_to_open != NULL)
      *failed_to_open = true;
    
    return NIL; // maybe return an ERROR instead.
  }
  else if (failed_to_open != NULL) {
    *failed_to_open = false;
  }

  char * const file_basename = basename((char *)filename);
  char * const last_loaded_file_str = free_list_malloc(strlen(file_basename) + 1);
  strcpy(last_loaded_file_str, file_basename);

  ae_obj_t * loaded_file = NEW_STRING(last_loaded_file_str);
  last_loaded_file = loaded_file;
  
  yylineno = 0;
  yyrestart(yyin);
  yyparse();

  // ae_obj_t * ret = EVAL(env, program);
  
  fclose(yyin);

  yyin = original_yyin;

  last_loaded_file = NIL;

  PUT_PROP_RAW(KW("program"), loaded_file, program);
  
  return program; 
}

