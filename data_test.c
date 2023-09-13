#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae_obj.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Twenty Four";
static char * three = "Three";

ae_obj_t * map_fun(const ae_obj_t * const obj) {
  ae_obj_t * new_obj = ALLOC_AE_OBJ;
  ae_obj_init(new_obj, AE_INTEGER_);
  
  printf("Measuring \"%s\".\n", obj->str_value);
  new_obj->int_value = strlen(obj->str_value);

  printf("Set ");
  ae_obj_put(new_obj);
  putchar('\n');
  
  return new_obj;
}

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
  ae_obj_init(list, AE_CONS____);

#ifdef NOISY_INIT
  puts("Done initting list.\n");
#endif
  
  ae_obj_push_back(list, obj_one);
  ae_obj_push_back(list, obj_two);
  ae_obj_push_back(list, obj_three);
  ae_obj_push_back(list, obj_four);
    
  putchar('\n');

  if (list->type == AE_CONS____ && list->head)
    ae_obj_each(list, describe);

  putchar('\n');
    
  ae_obj_t * clone = ae_obj_clone(list);

  ae_obj_t * prepended = ALLOC_AE_OBJ;
  ae_obj_init(prepended, AE_CONS____);
  prepended->head = obj_two;
  prepended->tail = clone;
  clone = prepended;
  
  clone = ae_obj_cons(obj_three, clone);
  
  puts("\nPrinting clone.");

  if (clone->type == AE_CONS____ && clone->head)
    ae_obj_each(clone, describe);

  puts("Done printing clone.\n");

  ae_obj_t * mapped = ae_obj_map(clone, map_fun);
  
  puts("\nPrinting mapped.");

  if (mapped->type == AE_CONS____ && mapped->head)
    ae_obj_each(mapped, describe);

  puts("Done printing mapped.");

  //return 0;

  extern ae_obj_t pool[POOL_SIZE];

  puts("\nPrinting pool contents.");
  for (size_t ix = 0; ix < POOL_SIZE; ix++) {
    printf("# %5d: ", ix); 
    ae_obj_put(&pool[ix]);
    putchar('\n');
  }
  puts("Printed pool contents.");
}
