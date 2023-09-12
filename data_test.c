#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae_obj.h"

static char * zero  = "Zer";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Thr";

void describe(ae_obj_t * ae_obj_p) {
  ae_obj_t * ae_obj = ae_obj_p;
  ae_obj_put((ae_obj_t *)ae_obj_p);
  putchar('\n');
}

int main() {
  putchar('\n');

  ae_obj_t * obj_one = ALLOC_AE_OBJ;
  ae_obj_init(obj_one, AE_STRING);
  obj_one->str_value = zero;
  ae_obj_t * obj_two = ALLOC_AE_OBJ;
  ae_obj_init(obj_two, AE_STRING);
  obj_two->str_value = one;
  ae_obj_t * obj_three = ALLOC_AE_OBJ;
  ae_obj_init(obj_three, AE_STRING);
  obj_three->str_value = two;
  ae_obj_t * obj_four = ALLOC_AE_OBJ;
  ae_obj_init(obj_four, AE_STRING);
  obj_four->str_value = three;

  puts("Done initting strings.\n");
  
  ae_obj_t * list = ALLOC_AE_OBJ;
  ae_obj_init(list, AE_LIST);

  puts("Done initting list.\n");
  
  fputs("Pushing ", stdout);
  ae_obj_put(obj_one);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  ae_obj_push_back(list, obj_one);
  
  fputs("Pushing ", stdout);
  ae_obj_put(obj_two);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  ae_obj_push_back(list, obj_two);
  
  fputs("Pushing ", stdout);
  ae_obj_put(obj_three);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  ae_obj_push_back(list, obj_three);
  
  fputs("Pushing ", stdout);
  ae_obj_put(obj_four);
  fputs(" into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  ae_obj_push_back(list, obj_four);

  fputs("\nDone pushing into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  putchar('\n');
    
  if (list->type == AE_LIST && list->head)
    ae_obj_each(list, describe);

  putchar('\n');
    
  ae_obj_t * clone = ae_obj_clone(list);

  printf("\nPrint clone\n");

  if (clone->type == AE_LIST && clone->head)
    ae_obj_each(clone, describe);
}
