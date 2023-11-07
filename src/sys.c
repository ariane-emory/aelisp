#include <assert.h>
#include <stdbool.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <unistd.h>

#include "sys.h"

#include "free_list.h"

#define BUFFER_SIZE 4096

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_read_from_fd
////////////////////////////////////////////////////////////////////////////////////////////////////
typedef enum {
  RFFD_OK,
  RFFD_NO_ALLOC,
} read_from_fd_state_t;
////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct read_from_fd_t {
  read_from_fd_state_t state;
  char * buffer;
  size_t size;
} read_from_fd_t;
////////////////////////////////////////////////////////////////////////////////////////////////////
static read_from_fd_t ae_sys_read_from_fd(int fd) {
  assert(fd);
  
  read_from_fd_t result;
  memset(&result, 0, sizeof(result));

  char buffer[BUFFER_SIZE];
  char * output     = NULL;
  size_t total_read = 0;

  while (true) {
    int bytes_read = read(fd, buffer, sizeof(buffer) - 1); // -1 to leave space for null terminator
    
    if (bytes_read <= 0) // Nothing more to read or an error occurred
      break;

    buffer[bytes_read] = '\0'; // Null-terminate the buffer

    char * const new_output = free_list_malloc(total_read + bytes_read + 1); // +1 for null terminator

    if (! new_output) {
      result.state = RFFD_NO_ALLOC;
      return result;
    }
    
    if (output) {
      memcpy(new_output, output, total_read);
      free_list_free(output);
    }

    memcpy(new_output + total_read, buffer, bytes_read);
    output      = new_output;
    total_read += bytes_read;
  }

  if (output) {
    output[total_read] = '\0'; // Ensure null-termination
  }

  result.state  = RFFD_OK; 
  result.buffer = output;
  result.size   = total_read;  

  return result;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_capture_command_output
////////////////////////////////////////////////////////////////////////////////////////////////////
captured_command_output_t ae_sys_capture_command_output(char * const command) {
  assert(command);
  
  captured_command_output_t result;
  memset(&result, 0, sizeof(result));

  int  stdout_pipe[2];
  int  stderr_pipe[2];
  pid_t pid;

  if (pipe(stdout_pipe) || pipe(stderr_pipe)) {
    result.state = CCOS_STATE_NO_PIPE;
    return result;
  }

  if ((pid = fork()) == -1) {
    result.state = CCOS_STATE_NO_FORK;
    return result;
  }

  if (pid == 0) { // Child process.
    close(stdout_pipe[0]);
    close(stderr_pipe[0]);

    dup2(stdout_pipe[1], STDOUT_FILENO);
    dup2(stderr_pipe[1], STDERR_FILENO);

    close(stdout_pipe[1]);
    close(stderr_pipe[1]);

    execl("/bin/bash", "bash", "-c", command, (char *)NULL);
    // If execl() fails.
    _exit(1);
  }

  // Parent process
  close(stdout_pipe[1]);
  close(stderr_pipe[1]);

  read_from_fd_t stdout_result = ae_sys_read_from_fd(stdout_pipe[0]);
  read_from_fd_t stderr_result = ae_sys_read_from_fd(stderr_pipe[0]);

  close(stdout_pipe[0]);
  close(stderr_pipe[0]);

  int status;
  waitpid(pid, &status, 0);

  result.state       = CCOS_STATE_COMPLETED;
  result.stdout      = stdout_result.buffer;
  result.stdout_size = stdout_result.size;
  result.stderr      = stderr_result.buffer;
  result.stderr_size = stderr_result.size;
  result.exit        = WEXITSTATUS(status);

  return result;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_expand_tilde
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_sys_expand_tilde(const char * const path, char ** expanded_path) {
  *expanded_path = NULL;
  
  if (! path || path[0] != '~')
    return false; 

  const char * const home = getenv("HOME");
    
  if (! home)
    return false;

  const size_t len = strlen(home) + strlen(path);
    
  *expanded_path = free_list_malloc(len);

  if (! expanded_path)
    return false;

  strcpy(*expanded_path, home);
  strcat(*expanded_path, path + 1);

  return true;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_file_exists
////////////////////////////////////////////////////////////////////////////////////////////////////
bool ae_sys_file_exists(const char * const filename) {
  struct stat buffer;
    
  int exist = stat(filename, &buffer);

  return exist == 0;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// _fread_string
////////////////////////////////////////////////////////////////////////////////////////////////////
fread_string_t ae_sys_file_read_string(const char * const filename) {
  assert(filename);
  
  fread_string_t result;
  memset(&result, 0, sizeof(result));

  FILE * const file = fopen(filename, "r");

  if (! file) {
    result.state = FRS_NO_OPEN;
    return result;
  }

  fseek(file, 0, SEEK_END);
  long filesize = ftell(file);
  rewind(file);

  char * const buffer = free_list_malloc(filesize + 1);

  if (! buffer) {
    result.state = FRS_NO_ALLOC;
    return result;
  }

  size_t read = fread(buffer, sizeof(char), filesize, file);

  buffer[read] = '\0';
  fclose(file);

  result.state  = FRS_READ;
  result.buffer = buffer;
  result.length = read;

  return result;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
