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

ae_obj_t * program = NIL;

////////////////////////////////////////////////////////////////////////////////////////////////////
// preface
////////////////////////////////////////////////////////////////////////////////////////////////////

void preface(void) {
  NL;
  printf("obj size:          %d.\n",    sizeof(ae_obj_t));
  printf("int size:          %d.\n",    sizeof(int));
  printf("nil is at:         %016p.\n", NIL);
  printf("t is at:           %016p.\n", TRUE);
  printf("Pool first:        %016p.\n", pool_first);
  printf("Pool last:         %016p.\n", pool_last);
  printf("Pool size:         %016p (%zu bytes).\n",
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE,
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE);
  printf("Strings pool size: %016p (%zu bytes).", free_list_size, free_list_size);
  NL;
}

ae_obj_t * load_file(ae_obj_t * const env, const char * filename, bool * const failed_to_open);

ae_obj_t * setup_root_env(void) {

#ifdef AE_PREFACE
  preface();
#endif
  
////////////////////////////////////////////////////////////////////////////////////////////////////
// set up the free list and populate the root env
////////////////////////////////////////////////////////////////////////////////////////////////////
  
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
      
#ifdef AE_DEBUG_OBJ
      DOBJ(pool_get_object(ix)) = primordial_origin;
#endif
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
  char * const       path            = free_list_malloc(PATH_MAX);
  uint32_t           size            = PATH_MAX;
  
  if (_NSGetExecutablePath(path, &size) == 0) {
    char * const tmp = free_list_malloc(strlen(dirname(path))+1);

    strcpy(tmp, dirname(path));
    strcpy(path, tmp);
    strcat(path, stdlib_rel_path);
    
    free_list_free(tmp);

    PR("Loading stdlib from %s... ", path);
  } else {
    FPR(stderr, "Buffer too small, need %d bytes!\n", size);
    
    exit(1);
  }

  bool failed_to_load = false;
  
  load_file(root_env, path, &failed_to_load);

  if (failed_to_load)
    FPR(stderr, "WARNING: Failed to load stdlib!\n");
  else
    PR("loaded.\n");

  free_list_free(path);
  
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
    if (! FREEP(pool_get_object(ix)) && ! DHAS(pool_get_object(ix), "origin")) {
#ifdef AE_LOG_KVP_SET_GET
      //  LOG(pool_get_object(ix), "#%d: Setting origin to 'read'", ix);
#endif
      
#ifdef AE_DEBUG_OBJ
      DOBJ(pool_get_object(ix)) = read_origin;
#endif
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

ae_obj_t * load_file(ae_obj_t * const env, const char * filename, bool * const failed_to_open) {
  FILE * original_yyin = yyin;
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
  
  yyrestart(yyin);
  yyparse();

  ae_obj_t * ret = EVAL(env, program);
  
  fclose(yyin);

  yyin = original_yyin;

  return ret; 
}

