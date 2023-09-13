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

#ifdef NOISY_INIT
  puts("Initting strings.");
#endif

  ae_obj_t * obj_one = ALLOC_AE_OBJ;
  ae_obj_init(obj_one, AE_STRING__);
  obj_one->str_value = zero;
  ae_obj_t * obj_two = ALLOC_AE_OBJ;
  ae_obj_init(obj_two, AE_STRING__);
  obj_two->str_value = one;
  ae_obj_t * obj_three = ALLOC_AE_OBJ;
  ae_obj_init(obj_three, AE_STRING__);
  obj_three->str_value = two;
  ae_obj_t * obj_four = ALLOC_AE_OBJ;
  ae_obj_init(obj_four, AE_STRING__);
  obj_four->str_value = three;

#ifdef NOISY_INIT
  puts("Done initting strings.\n");
#endif

#ifdef NOISY_INIT
  puts("Initting list.");
#endif

  ae_obj_t * list = ALLOC_AE_OBJ;
  ae_obj_init(list, AE_LIST);

#ifdef NOISY_INIT
  puts("Done initting list.\n");
#endif
  
  ae_obj_push_back(list, obj_one);
  ae_obj_push_back(list, obj_two);
  ae_obj_push_back(list, obj_three);
  ae_obj_push_back(list, obj_four);
    
  putchar('\n');

  if (list->type == AE_LIST && list->head)
    ae_obj_each(list, describe);

  putchar('\n');
    
  ae_obj_t * clone = ae_obj_clone(list);

  puts("\nPrinting clone.");

  if (clone->type == AE_LIST && clone->head)
    ae_obj_each(clone, describe);

  puts("Done printing clone.");
}
