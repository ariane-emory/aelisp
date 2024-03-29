#include <dirent.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syslimits.h>
#include <time.h>
#include <unistd.h>

#include "core_includes.h"
#include "common.h"
#include "env.h"
#include "free_list.h"
#include "sys.h"
#include "sys_time.h"
#include "pool.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// wrap_captured_command_output
///////////////////////////////////////////////////////////////////////////////////////////////////
static ae_obj_t * wrap_captured_command_output(capture_command_output_t captured) {
  if (captured.state != CCOS_STATE_OK)
    switch (captured.state) {
    case CCOS_STATE_NO_EXEC:
      return NEW_ERROR("Execution failed");
    case CCOS_STATE_NO_PIPE:
      return NEW_ERROR("Pipe creation failed");
    case CCOS_STATE_NO_FORK:
      return NEW_ERROR("Fork failed");
    default:
      return NEW_ERROR("Unknown error");
    }
 
  ae_obj_t * const plist =
    CONS(KW("exit"),
         CONS(NEW_INT(captured.exit),
              CONS(KW("stdout"),
                   CONS(captured.stdout ? NEW_STRING(captured.stdout) : NIL,
                        CONS(KW("stderr"),
                             CONS(captured.stderr ? NEW_STRING(captured.stderr) : NIL,
                                  NIL))))));

  return plist;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _system
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(system) {
  REQUIRE(STRINGP(CAR(args)),
          "system's arg must be a string");

  RETURN(wrap_captured_command_output(ae_sys_capture_command_output(STR_VAL(CAR(args)))));

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _expand_path
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(expand_path) {
  REQUIRE(STRINGP(CAR(args)));

  char * path                = STR_VAL(CAR(args));
  char * expanded_tilde_path = NULL;
  bool   expanded_tilde      = ae_sys_expand_tilde(path, &expanded_tilde_path);

  if (expanded_tilde)
    path = expanded_tilde_path;
  
  char * const realpath_tmp = free_list_malloc(PATH_MAX);
  realpath(path, realpath_tmp);
  
  char * const tmp = free_list_malloc(strlen(realpath_tmp) + 1);
  strcpy(tmp, realpath_tmp);
  free_list_free(realpath_tmp);
  
  if (expanded_tilde)
    free_list_free(expanded_tilde_path);

  path = tmp;
  
  RETURN(NEW_STRING(path));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _allocated
///////////////////////////////////////////////////////////////////////////////////////////////////s
DEF_CORE_FUN(allocated) {
  RETURN(NEW_INT(pool_allocated));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _now
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(now) {
  RETURN(NEW_INT(ae_sys_time_now()));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _now_us
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(now_us) {
  RETURN(NEW_INT(ae_sys_time_now_us()));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(elapsed) {
  REQUIRE(INTEGERP(CAR(args)));
  RETURN(NEW_INT(ae_sys_time_elapsed(INT_VAL(CAR(args)))));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed_us
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(elapsed_us) {
  REQUIRE(INTEGERP(CAR(args)));
  RETURN(NEW_INT(ae_sys_time_elapsed_us(INT_VAL(CAR(args)))));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _sleep
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(sleep) {
  REQUIRE(INTEGERP(CAR(args)));

  usleep(INT_VAL(CAR(args)) * 1000);

  RETURN(CAR(args));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _sleep_us
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(sleep_us) {
  REQUIRE(INTEGERP(CAR(args)));

  usleep(INT_VAL(CAR(args)));

  RETURN(CAR(args));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(exit) {
  int exit_code = 0;

  if (args_length == 1) {
    REQUIRE(INTEGERP(CAR(args)));

    exit_code = INT_VAL(CAR(args));
  }
  
  exit(exit_code);

  RETURN(CAR(args));
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// find_file_in_load_path
///////////////////////////////////////////////////////////////////////////////////////////////////
static char * find_file_in_load_path(ae_obj_t * const env,
                                     bool add_extension,
                                     const char * const name) {
  bool             load_path_found = false;
  ae_obj_t * const load_path       = ENV_GET(env, SYM("*load-path*"), &load_path_found);

  assert(load_path_found);
  assert(load_path);
  
  FOR_EACH(dir, load_path) {
    if (! ae_sys_dir_exists(STR_VAL(dir)))
      FPR(stderr, "WARNING: load path directory '%s' does not exist\n", STR_VAL(dir));
    
    char * const possible_path = free_list_malloc(strlen(STR_VAL(dir)) +
                                                  strlen(name) +
                                                  (add_extension ? 7 : 2));
    
    sprintf(possible_path,
            add_extension ? "%s/%s.lisp" : "%s/%s",
            STR_VAL(dir), name);

    if (ae_sys_file_exists(possible_path))
      return possible_path;
    else
      free_list_free(possible_path);
  }

  return NULL;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// have_feature
///////////////////////////////////////////////////////////////////////////////////////////////////
static bool have_feature(ae_obj_t * const env, ae_obj_t * const sym) {
  assert(sym);
  assert(SYMBOLP(sym) && (! KEYWORDP(sym)));
  assert(env);
  assert(ENVP(env));

  bool       features_found = false;
  ae_obj_t * const features = ENV_GET(env, SYM("*features*"), &features_found);

  assert(features_found);
  assert(features);
  assert(TAILP(features));
  
  FOR_EACH(feature, features)
    if (EQL(feature, sym))
      return true;
  
  return false;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// load_or_require
///////////////////////////////////////////////////////////////////////////////////////////////////
typedef enum {
  LORM_READ_FILE,
  LORM_LOAD,
  LORM_REQUIRE,
  LORM_REREQUIRE,
} load_or_require_mode_t;

static ae_obj_t * load_or_require(load_or_require_mode_t mode,
                                  ae_obj_t * const env,
                                  ae_obj_t * const args,
                                  __attribute__((unused)) int args_length) {
  CORE_BEGIN("load_or_require");

  ae_obj_t * const load_target = CAR(args);
  
  if ((mode == LORM_REQUIRE) && have_feature(env, load_target))
    RETURN(load_target);

  // Args will have already been checked by the caller, so don't bother doing this:
  // REQUIRE((SYMBOLP(load_target) || ! KEYWORDP(load_target)) || STRINGP(load_target));

  char * const load_target_string = SYMBOLP(load_target) ? SYM_VAL(load_target) : STR_VAL(load_target);
  char * const file_path =
    mode == LORM_READ_FILE
    ? load_target_string
    : find_file_in_load_path(env, mode != LORM_LOAD, load_target_string);
    
  bool no_error = (args_length == 2) && ! NILP(CADR(args));

  if ((! file_path) || (! ae_sys_file_exists(file_path)))
    RETURN(no_error ? NIL : NEW_ERROR("could not find file for '%s", load_target_string));
  
  ae_obj_t * const new_program = RETURN_IF_ERRORP(load_file(file_path, NULL));

  if (mode != LORM_READ_FILE)
    free_list_free(file_path);

  ret = mode == LORM_READ_FILE ? new_program : RETURN_IF_ERRORP(EVAL(env, new_program));

  if ((mode == LORM_REQUIRE || mode == LORM_REREQUIRE) && ! have_feature(env, load_target))
    RETURN(NEW_ERROR("required file did not provide '%s", load_target_string));
  
  CORE_END("load_or_require");
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _load
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(load) {
  REQUIRE(STRINGP(CAR(args)));
  RETURN(load_or_require(LORM_LOAD, env, args, args_length));
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(file_read) {
  REQUIRE(STRINGP(CAR(args)));
  RETURN(CDR(load_or_require(LORM_READ_FILE, env, args, args_length)));  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _require
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(require) {
  REQUIRE(SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  RETURN(load_or_require(LORM_REQUIRE, env, args, args_length));
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////

   
///////////////////////////////////////////////////////////////////////////////////////////////////
// _requireb
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(requireb) {
  REQUIRE(SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  RETURN(load_or_require(LORM_REREQUIRE, env, args, args_length));
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _cd
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(cd) {
  REQUIRE(STRINGP(CAR(args)),
          "cd's arg must be a string");

  char * const dst = STR_VAL(CAR(args));
  char         cwd[PATH_MAX];
  
  if (getcwd(cwd, sizeof(cwd)) == NULL)
    RETURN(NIL);
  
  REQUIRE(chdir(dst) == 0,
          "Could not change directory");
  
  if (strcmp(dst, "..") == 0 && strcmp(cwd, "/") == 0)
    RETURN(NIL);

  char * const pwd = ae_sys_pwd();

  RETURN((! pwd)
         ? NEW_ERROR("Could not get current working directory after changing directory")
         : NEW_STRING(pwd));

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _pwd
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(pwd) {
  char * const pwd = ae_sys_pwd();

  RETURN(pwd
         ? NEW_STRING(pwd)
         : NEW_ERROR("Could not get current working directory"));

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _basename
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(basename) {
  REQUIRE(STRINGP(CAR(args)),
          "basename's arg must be a string");

  char * const path = ae_sys_basename(STR_VAL(CAR(args)));

  RETURN(path
         ? NEW_STRING(path)
         : NEW_ERROR("Could not get basename"));

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _dirname
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(dirname) {
  REQUIRE(STRINGP(CAR(args)),
          "dirname's arg must be a string");

  char * const path = ae_sys_dirname(STR_VAL(CAR(args)));

  RETURN(path
         ? NEW_STRING(path)
         : NEW_ERROR("Could not get dirname"));

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _files
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(files) {
  REQUIRE((args_length == 0) || STRINGP(CAR(args)), "dirs's arg must be a string");
  
  char * path = NULL;

  if (args_length == 0) {
    path = ae_sys_pwd();

    if (! path)
      RETURN(NEW_ERROR("Could not get current working directory"));
  }
  else {
    path = STR_VAL(CAR(args));
  }
  
  DIR * const dir = opendir(path);

  REQUIRE(dir,
          "could not open directory");

  struct dirent * entry;
  
  while ((entry = readdir(dir))) {
    if (entry->d_type == DT_REG) {
      char * const new_string = free_list_malloc(strlen(entry->d_name) + 1);

      if (! new_string) {
        closedir(dir);

        RETURN(NEW_ERROR("Out of memory when allocating new string for filename"));
      }

      strcpy(new_string, entry->d_name);

      ret = CONS(NEW_STRING(new_string), ret);
    }
  }

  closedir(dir);

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _dirs
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(dirs) {
  REQUIRE((args_length == 0) || STRINGP(CAR(args)), "dirs's arg must be a string");
  
  char * path = NULL;

  if (args_length == 0) {
    path = ae_sys_pwd();

    if (! path)
      RETURN(NEW_ERROR("Could not get current working directory"));
  }
  else {
    path = STR_VAL(CAR(args));
  }
  
  DIR * const dir = opendir(path);

  REQUIRE(dir,
          "could not open directory");

  struct dirent * entry;
  
  while ((entry = readdir(dir)))
    if (entry->d_type == DT_DIR && strcmp(entry->d_name, ".") != 0) {
      char * const new_string = free_list_malloc(strlen(entry->d_name) + 1);

      if (! new_string) {
        closedir(dir);

        RETURN(NEW_ERROR("Out of memory when allocating new string for directory name"));
      }

      strcpy(new_string, entry->d_name);
      
      ret = CONS(NEW_STRING(new_string), ret);
    }

  closedir(dir);

  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read_string
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(fwrite_string) {
  REQUIRE(STRINGP(CAR(args)) && STRINGP(CADR(args)),
          "Arguments must be strings");

  char * const filename  = STR_VAL(CAR(args));
  char * const data      = STR_VAL(CADR(args));
  FILE * const file      = fopen(filename, "w");

  REQUIRE(file,
          "Could not open file for writing");

  size_t data_size = strlen(data);
  size_t written   = fwrite(data, sizeof(char), data_size, file);
  
  fclose(file);

  RETURN(TRUTH(written == data_size));
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_append_string
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(fappend_string) {
  REQUIRE(STRINGP(CAR(args)) && STRINGP(CADR(args)),
          "Arguments must be strings");

  char * const filename  = STR_VAL(CAR(args));
  char * const data      = STR_VAL(CADR(args));
  FILE * const file      = fopen(filename, "a");
  
  REQUIRE(file, "Could not open file for appending");

  size_t data_size = strlen(data);
  size_t written   = fwrite(data, sizeof(char), data_size, file);
  
  fclose(file);

  RETURN(TRUTH(written == data_size));
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read_string
///////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(fread_string) {
  REQUIRE(STRINGP(CAR(args)),
          "Argument must be a string");

  fread_string_t result = ae_sys_file_read_string(STR_VAL(CAR(args)));

  switch (result.state) {
  case FRS_NO_ALLOC:
    ret = NEW_ERROR("Could not allocate memory for file read");

    break;
  case FRS_NO_OPEN:
    ret = NEW_ERROR("Could not open file for reading");

    break;
  default:
    ret = NEW_STRING(result.buffer);
  }

  RETURN(ret);
  
  END_DEF_CORE_FUN;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
