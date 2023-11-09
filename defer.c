#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CONCAT2(x, y) x##y
#define CONCAT(x, y) CONCAT2(x, y)

#define defer(var_decl, cleanup_stmts)                                                             \
  var_decl;                                                                                        \
  for (int CONCAT(_defer_flag_, __LINE__) = 1;                                                     \
       CONCAT(_defer_flag_, __LINE__);                                                             \
       ({                                                                                          \
         (CONCAT(_defer_flag_, __LINE__) = 0);                                                     \
         cleanup_stmts; (void)0;                                                                   \
       }))

int main() {
  defer(char * str, free(str); str = NULL;) {
    str = malloc(6);
    strcpy(str, "hello world");
    printf("%s\n", str);
  }
  
  if (! str) {
    printf("str is NULL\n");
  }
}

