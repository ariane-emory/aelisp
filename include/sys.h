#pragma once

#include <stdbool.h>

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef enum {
  FRS_OK,
  FRS_NO_ALLOC,
  FRS_NO_OPEN
} fread_string_state_t;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct fread_string_t {
  fread_string_state_t state;
  char *               buffer;
  size_t               length;
} fread_string_t;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef enum {
  CCOS_STATE_OK,
  CCOS_STATE_NO_EXEC,
  CCOS_STATE_NO_PIPE,
  CCOS_STATE_NO_FORK,
} capture_command_output_state_t;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct capture_command_output_t {
  capture_command_output_state_t state;
  char *                         stdout;
  size_t                         stdout_size;
  char *                         stderr;
  size_t                         stderr_size;
  int                            exit;
} capture_command_output_t;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
capture_command_output_t ae_sys_capture_command_output(const char * const command);
bool                     ae_sys_expand_tilde(const char * const path, char ** expanded_path);
bool                     ae_sys_dir_exists(const char * const pathname);
bool                     ae_sys_file_exists(const char * const filename);
fread_string_t           ae_sys_file_read_string(const char * const filename);
char *                   ae_sys_pwd();
char *                   ae_sys_basename(const char * const path);
char *                   ae_sys_dirname(const char * const path);
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
