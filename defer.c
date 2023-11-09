#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Helper macros for unique label generation
#define CONCAT_INTERNAL(x, y) x##y
#define CONCAT(x, y) CONCAT_INTERNAL(x, y)

// Macro to create a unique label for deferred execution
#define DEFER(stmt)                                                                                \
  for (int CONCAT(_defer_flag_, __LINE__) = 1;                                                     \
       CONCAT(_defer_flag_, __LINE__);                                                             \
       (CONCAT(_defer_flag_, __LINE__) = 0), stmt)                                                 


int main() {
  char * hello = malloc(6);
  strcpy(hello, "hello");
  printf("%s\n", hello);

  DEFER(printf("cleanup.\n")) {
    printf("work.\n");
  }

  printf("%s\n", ({ printf("beep.\n"); "boop"; }));
  
  return 0;
}


/*
  for (int _defer_flag_21 = 1;
           _defer_flag_21;
           (_defer_flag_21 = 0), printf("cleanup.\n")) {
    printf("work.\n");
  }
*/
