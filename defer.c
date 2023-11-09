#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define until(expr) while (! (expr))
#define unless(expr) if (! (expr))

#define CONCAT2(x, y) x##y
#define CONCAT(x, y) CONCAT2(x, y)

#define defer(var_decl, cleanup_stmts)                                                             \
  var_decl;                                                                                        \
  for (int CONCAT(_defer_flag_, __LINE__) = 1;                                                     \
       CONCAT(_defer_flag_, __LINE__);                                                             \
       ({                                                                                          \
         (CONCAT(_defer_flag_, __LINE__) = 0);                                                     \
         cleanup_stmts;                                                                            \
         (void)0;                                                                                  \
       }))

#define raii_charp(name) char * name, free(name); name = NULL

#define FREE(var) ({ free((var)); (var) = NULL; })

int main() {
  // defer (raii_charp(str))
  defer (char * str, FREE(str)) {
    str = malloc(6);
    strcpy(str, "hello world");
    printf("%s\n", str);
  }
  
  unless (str) {
    printf("str is NULL\n");
  }
}

