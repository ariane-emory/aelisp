#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Helper macros for unique label generation
#define CONCAT_INTERNAL(x, y) x##y
#define CONCAT(x, y) CONCAT_INTERNAL(x, y)

// Macro to create a unique label for deferred execution
#define DEFER(stmt, stmt2)                                                                         \
  for (int CONCAT(_defer_flag_, __LINE__) = 1;                                                     \
       CONCAT(_defer_flag_, __LINE__);                                                             \
       ({ (CONCAT(_defer_flag_, __LINE__) = 0); stmt; }) )                                                 

#define DEFER2(var_decl, cleanup)                                                                  \
  var_decl;                                                                                        \
                                                                                                   \
  for (int CONCAT(_defer_flag_, __LINE__) = 1;                                                     \
       CONCAT(_defer_flag_, __LINE__);                                                             \
       ({ (CONCAT(_defer_flag_, __LINE__) = 0); cleanup; (void)0; }) )                              \
              


int main() {
  char * hello = malloc(6);
  strcpy(hello, "hello");
  printf("%s\n", hello);

  DEFER(printf("cleanup.\n"); printf("more cleanup.\n"), printf("ignored cleanup.\n")) {
    printf("work.\n");
  }

  DEFER2(char * str, free(str)) {
    str = malloc(6);
    strcpy(str, "magic");
    printf("%s\n", str);
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
