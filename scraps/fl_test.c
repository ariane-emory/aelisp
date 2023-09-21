// gcc -Iinclude scraps/fl_test.c src/ae_free_list.c -o ./bin/fl_test && ./bin/fl_test
#include "ae_free_list.h"
#include "stdio.h"
#include "stdbool.h"
#include "string.h"

#define free_list_size 1 << 20

static char mem[free_list_size] = { 0 };
  
int main() {
    free_list_add_block(&mem[0], free_list_size);

    while (true) 
      for (int ix = 0; ix < 32000; ix++) {
        char * str = (char*)free_list_malloc(1 << 7);

        if (! str) {
          free_list_reset();
          memset(mem, 0, free_list_size);
          free_list_add_block(mem, free_list_size);
        
          continue;
        } 
        strcpy(str, "hello world.");
        // puts(str);
      }
}
