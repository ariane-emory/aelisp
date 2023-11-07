#include <stdbool.h>
#include <string.h>

#include "sys_funcs.h"

#define BUFFER_SIZE 4096

////////////////////////////////////////////////////////////////////////////////////////////////////
// read_from_fd
////////////////////////////////////////////////////////////////////////////////////////////////////
char * read_from_fd(int fd, size_t * const size) {
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
