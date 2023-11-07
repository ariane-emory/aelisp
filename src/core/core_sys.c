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
  if (captured.state != CCOS_STATE_COMPLETED)
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
 
  // If completed successfully, create the plist
  ae_obj_t * const plist = CONS(KW("exit"),
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
ae_obj_t * ae_core_system(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("system");

  REQUIRE(env, args, STRINGP(CAR(args)), "system's arg must be a string");

  ret = wrap_captured_command_output(ae_sys_capture_command_output(STR_VAL(CAR(args))));
  
  CORE_RETURN("system", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _expand_path
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_expand_path(ae_obj_t * const env,
                               ae_obj_t * const args,
                               __attribute__((unused)) int args_length) {
  CORE_BEGIN("expand-path");

  REQUIRE(env, args, STRINGP(CAR(args)));

  char * path = STR_VAL(CAR(args));
  
  char * expanded_tilde_path = NULL;
  bool   expanded_tilde      = ae_sys_expand_tilde(path, &expanded_tilde_path);

  if (expanded_tilde)
    path = expanded_tilde_path;
  
  char * realpath_tmp = free_list_malloc(PATH_MAX);
  realpath(path, realpath_tmp);
  
  char * tmp = free_list_malloc(strlen(realpath_tmp) + 1);
  strcpy(tmp, realpath_tmp);
  free_list_free(realpath_tmp);
  
  if (expanded_tilde)
    free_list_free(expanded_tilde_path);

  path = tmp;
  
  ret = NEW_STRING(path);
  
  CORE_RETURN("expand-path", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _allocated
///////////////////////////////////////////////////////////////////////////////////////////////////s
ae_obj_t * ae_core_allocated(__attribute__((unused)) ae_obj_t * const env,
                             __attribute__((unused)) ae_obj_t * const args,
                             __attribute__((unused)) int args_length) {
  CORE_BEGIN("allocated");
  CORE_RETURN("allocated", NEW_INT(pool_allocated));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _now
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_now(__attribute__((unused)) ae_obj_t * const env,
                       __attribute__((unused)) ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("now");
  CORE_RETURN("now", NEW_INT(ae_sys_time_now()));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _now_us
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_now_us(__attribute__((unused)) ae_obj_t * const env,
                          __attribute__((unused)) ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("now_us");
  CORE_RETURN("now_us", NEW_INT(ae_sys_time_now_us()));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_elapsed(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("elapsed");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  CORE_RETURN("elapsed", NEW_INT(ae_sys_time_elapsed(INT_VAL(CAR(args)))));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _elapsed_us
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_elapsed_us(ae_obj_t * const env,
                              ae_obj_t * const args,
                              __attribute__((unused)) int args_length) {
  CORE_BEGIN("elapsed_us");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  CORE_RETURN("elapsed", NEW_INT(ae_sys_time_elapsed_us(INT_VAL(CAR(args)))));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _sleep
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_sleep(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("sleep");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  usleep(INT_VAL(CAR(args)) * 1000);
  CORE_RETURN("sleep", CAR(args));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _sleep_us
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_sleep_us(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("sleep-us");
  REQUIRE(env, args, INTEGERP(CAR(args)));
  usleep(INT_VAL(CAR(args)));
  CORE_RETURN("sleep-us", CAR(args));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _exit
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_exit(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("exit");
  
  int exit_code = 0;

  if (args_length == 1) {
    REQUIRE(env, args, INTEGERP(CAR(args)));
    exit_code = INT_VAL(CAR(args));
  }
  
  exit(exit_code);
  
  CORE_RETURN("exit", CAR(args));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// find_file_in_load_path
///////////////////////////////////////////////////////////////////////////////////////////////////
static char * find_file_in_load_path(ae_obj_t * const env,
                                     bool add_extension,
                                     const char * const name) {
  bool load_path_found = false;
  ae_obj_t * const load_path = ENV_GET(env, SYM("*load-path*"), &load_path_found);

  assert(load_path_found);
  assert(load_path);
  
  FOR_EACH(dir, load_path) {
    char * const possible_path = add_extension
      ? free_list_malloc(strlen(STR_VAL(dir)) + strlen(name) + 7)
      : free_list_malloc(strlen(STR_VAL(dir)) + strlen(name) + 2);
    
    sprintf(possible_path,
            add_extension ? "%s/%s.lisp" : "%s/%s",
            STR_VAL(dir), name);

    if (access(possible_path, F_OK) != -1)
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

  bool features_found = false;
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
  // REQUIRE(env, args, (SYMBOLP(load_target) || ! KEYWORDP(load_target)) || STRINGP(load_target));

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
  
end:
  
  CORE_RETURN("load_or_require", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _load
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_load(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("load");

  REQUIRE(env, args, STRINGP(CAR(args)));

  CORE_RETURN("load", load_or_require(LORM_LOAD, env, args, args_length));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_file_read(ae_obj_t * const env,
                             ae_obj_t * const args,
                             __attribute__((unused)) int args_length) {
  CORE_BEGIN("file-read");

  REQUIRE(env, args, STRINGP(CAR(args)));

  CORE_RETURN("file-read", CDR(load_or_require(LORM_READ_FILE, env, args, args_length)));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _require
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_require(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("require");

  REQUIRE(env, args,
          SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  CORE_RETURN("require", load_or_require(LORM_REQUIRE, env, args, args_length));
}
///////////////////////////////////////////////////////////////////////////////////////////////////

   
///////////////////////////////////////////////////////////////////////////////////////////////////
// _requireb
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_requireb(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("requireb");

  REQUIRE(env, args,
          SYMBOLP(CAR(args))      &&
          (! KEYWORDP(CAR(args))) &&
          (! NILP(CAR(args)))     &&
          (! TRUEP(CAR(args))));

  CORE_RETURN("requireb", load_or_require(LORM_REREQUIRE, env, args, args_length));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _cd
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_cd(ae_obj_t * const env,
                      ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_BEGIN("cd");

  REQUIRE(env, args, STRINGP(CAR(args)), "cd's arg must be a string");

  char * const dst = STR_VAL(CAR(args));
  char         cwd[PATH_MAX];
  
  if (getcwd(cwd, sizeof(cwd)) == NULL)
    CORE_RETURN("cd", NIL);
  
  REQUIRE(env, args, chdir(dst) == 0, "Could not change directory");
  
  if (strcmp(dst, "..") == 0 && strcmp(cwd, "/") == 0)
    CORE_RETURN("cd", NIL);

  char * const pwd = ae_sys_pwd();

  if (! pwd)
    RETURN(NEW_ERROR("Could not get current working directory after changing directory"));
  
  ret = NEW_STRING(pwd);

end:
  
  CORE_RETURN("cd", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _pwd
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_pwd(ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("pwd");

  char * const pwd = ae_sys_pwd();

  if (! pwd)
    RETURN(NEW_ERROR("Could not get current working directory"));
  
  ret = NEW_STRING(pwd);
  
end:
  
  CORE_RETURN("pwd", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _basename
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_basename(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("basename");

  REQUIRE(env, args, STRINGP(CAR(args)), "basename's arg must be a string");

  char * const path = STR_VAL(CAR(args));
  char * const tmp = free_list_malloc(strlen(path) + 1);
  basename_r(path, tmp);
  char * const basename = free_list_malloc(strlen(tmp) + 1);
  strcpy(basename, tmp);
  free_list_free(tmp);
  
  ret = NEW_STRING(basename);
  
  CORE_RETURN("basename", ret);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// _dirname
///////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dirname(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("dirname");

  REQUIRE(env, args, STRINGP(CAR(args)), "dirname's arg must be a string");

  char * const path = STR_VAL(CAR(args));
  char * const tmp = free_list_malloc(strlen(path) + 1);
  dirname_r(path, tmp);
  char * const dirname = free_list_malloc(strlen(tmp) + 1);
  strcpy(dirname, tmp);
  free_list_free(tmp);
  
  ret = NEW_STRING(dirname);
    
  CORE_RETURN("dirname", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _files
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_files(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("files");

  REQUIRE(env, args, (args_length == 0) || STRINGP(CAR(args)), "dirs's arg must be a string");
  
  char * path = NULL;

  if (args_length == 0) {
    ae_obj_t * const pwd = RETURN_IF_ERRORP(ae_core_pwd(env, NIL, 0));
    path = STR_VAL(pwd);
  }
  else {
    path = STR_VAL(CAR(args));
  }
  
  DIR * const dir = opendir(path);
  REQUIRE(env, args, dir, "could not open directory");

  struct dirent * entry;
  
  while ((entry = readdir(dir))) {
    if (entry->d_type == DT_REG) {
      char * const new_string = free_list_malloc(strlen(entry->d_name) + 1);
      if (! new_string) {
        closedir(dir);
        RETURN(NEW_ERROR("Out of memory when allocating new string for filename"));
      }
      strcpy(new_string, entry->d_name);
      ret = CONS(NEW_STRING(new_string), ret); // Assuming NEW_STRING takes ownership of new_string
    }
  }

  closedir(dir);

end:
  
  CORE_RETURN("files", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _dirs
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_dirs(ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("dirs");

  REQUIRE(env, args, (args_length == 0) || STRINGP(CAR(args)), "dirs's arg must be a string");
  
  char * path = NULL;

  if (args_length == 0) {
    ae_obj_t * const pwd = RETURN_IF_ERRORP(ae_core_pwd(env, NIL, 0));
    path = STR_VAL(pwd);
  }
  else {
    path = STR_VAL(CAR(args));
  }
  
  DIR * const dir = opendir(path);

  REQUIRE(env, args, dir, "could not open directory");

  struct dirent * entry;
  
  while ((entry = readdir(dir)))
    if (entry->d_type == DT_DIR && strcmp(entry->d_name, ".") != 0) {
      char * const new_string = free_list_malloc(strlen(entry->d_name) + 1);
      if (! new_string) {
        closedir(dir);
        RETURN(NEW_ERROR("Out of memory when allocating new string for directory name"));
      }
      strcpy(new_string, entry->d_name);
      strcpy(new_string, entry->d_name);
      ret = CONS(NEW_STRING(new_string), ret);
    }

  closedir(dir);

end:
  
  CORE_RETURN("dirs", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read_string
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_fwrite_string(ae_obj_t * const env,
                                 ae_obj_t * const args,
                                 __attribute__((unused)) int args_length) {
  CORE_BEGIN("file-write-string");

  REQUIRE(env, args, STRINGP(CAR(args)) && STRINGP(CADR(args)), "Arguments must be strings");

  char *filename = STR_VAL(CAR(args));
  char *data     = STR_VAL(CADR(args));
  FILE *file     = fopen(filename, "w");

  REQUIRE(env, args, file, "Could not open file for writing");

  size_t written = fwrite(data, sizeof(char), strlen(data), file);
  
  fclose(file);

  CORE_RETURN("file-write-string", TRUTH(written == strlen(data)));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_append_string
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_fappend_string(ae_obj_t * const env,
                                  ae_obj_t * const args,
                                  __attribute__((unused)) int args_length) {
  CORE_BEGIN("file-append-string");

  REQUIRE(env, args, STRINGP(CAR(args)) && STRINGP(CADR(args)), "Arguments must be strings");

  char *filename = STR_VAL(CAR(args));
  char *data     = STR_VAL(CADR(args));
  FILE *file     = fopen(filename, "a");
  
  REQUIRE(env, args, file, "Could not open file for appending");

  size_t written = fwrite(data, sizeof(char), strlen(data), file);
  
  fclose(file);

  CORE_RETURN("file-append-string", TRUTH(written == strlen(data)));
}
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////
// _file_read_string
///////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t * ae_core_fread_string(ae_obj_t * const env,
                                ae_obj_t * const args,
                                __attribute__((unused)) int args_length) {
  CORE_BEGIN("file-read-string");

  REQUIRE(env, args, STRINGP(CAR(args)), "Argument must be a string");

  fread_string_t result = ae_sys_file_read_string(STR_VAL(CAR(args)));

  if (result.state == FRS_NO_ALLOC)
    ret = NEW_ERROR("Could not allocate memory for file read");
  else if (result.state == FRS_NO_OPEN)
    ret = NEW_ERROR("Could not open file for reading");
  else
    ret = NEW_STRING(result.buffer);

  CORE_RETURN("file-read-string", ret);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
