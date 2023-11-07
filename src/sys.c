#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>    // if you are using any IO functions like printf for debugging
#include <stdlib.h>   // if dynamic memory allocation occurs elsewhere in the function
#include <sys/types.h> // for pid_t
#include <sys/wait.h>  // for waitpid
#include <unistd.h>    // for fork, pipe, close, dup2, execl, _exit

#include "sys.h"

#include "free_list.h"

#define BUFFER_SIZE 4096

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_capture_command_output
////////////////////////////////////////////////////////////////////////////////////////////////////

captured_command_output_t ae_sys_capture_command_output(char * const command) {
  captured_command_output_t result;
  memset(&result, 0, sizeof(result)); // Initialize all fields to 0/NULL

  int stdout_pipe[2];
  int stderr_pipe[2];
  pid_t pid;
  size_t stdout_size = 0;
  size_t stderr_size = 0;
  char * stdout_output = NULL;
  char * stderr_output = NULL;

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

  stdout_output = ae_sys_read_from_fd(stdout_pipe[0], &stdout_size);
  stderr_output = ae_sys_read_from_fd(stderr_pipe[0], &stderr_size);

  close(stdout_pipe[0]);
  close(stderr_pipe[0]);

  int status;
  waitpid(pid, &status, 0);

  // Populate the result structure
  result.state = CCOS_STATE_COMPLETED;
  result.stdout = stdout_output; // Assume this is dynamically allocated and needs to be freed later
  result.stderr = stderr_output; // Assume this is dynamically allocated and needs to be freed later
  result.exit = WEXITSTATUS(status);

  return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_sys_read_from_fd
////////////////////////////////////////////////////////////////////////////////////////////////////
char * ae_sys_read_from_fd(int fd, size_t * const size) {
  char buffer[BUFFER_SIZE];
  char * output     = NULL;
  size_t total_read = 0;

  while (true) {
    int bytes_read = read(fd, buffer, sizeof(buffer) - 1); // -1 to leave space for null terminator
    
    if (bytes_read <= 0) // Nothing more to read or an error occurred
      break;

    buffer[bytes_read] = '\0'; // Null-terminate the buffer

    char * const new_output = free_list_malloc(total_read + bytes_read + 1); // +1 for null terminator
    
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

  *size = total_read;
  
  return output;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
