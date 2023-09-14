#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae_obj.h"

static char * zero  = "One";
static char * one   = "Two";
static char * two   = "Three";
static char * three = "Four";

ae_obj_t * map_fun_strlen(const ae_obj_t * const obj) {
  ae_obj_t * new_obj = NEW_AE_OBJ(AE_INTEGER_);
  
  printf("Measuring \"%s\".\n", obj->str_value);
  new_obj->int_value = strlen(obj->str_value);

  printf("Set ");
  ae_obj_put(new_obj);
  putchar('\n');
  
  return new_obj;
}

ae_obj_t * map_fun_clone(const ae_obj_t * const obj) {
  return ae_obj_clone(obj);
}

void describe(ae_obj_t * ae_obj_p) {
  ae_obj_t * ae_obj = ae_obj_p;
  ae_obj_put((ae_obj_t *)ae_obj_p);
  putchar('\n');
}

int main() {
  putchar('\n');

  ae_obj_t * list = 0;
  
  ae_obj_t * obj_four = NEW_AE_OBJ(AE_STRING__);
  obj_four->str_value = three;
  list = CONS(obj_four, list);
  
  ae_obj_t * obj_three = NEW_AE_OBJ(AE_STRING__);
  obj_three->str_value = two;
  list = CONS(obj_three, list);
  
  ae_obj_t * obj_two = NEW_AE_OBJ(AE_STRING__);
  obj_two->str_value = one;
  list = CONS(obj_two, list);
  
  ae_obj_t * obj_one = NEW_AE_OBJ(AE_STRING__);
  obj_one->str_value = zero;
  list = CONS(obj_one, list);
      
  putchar('\n');

  if (list->type == AE_CONS____ && list->head)
    ae_obj_each(list, describe);

  putchar('\n');
    
  ae_obj_t * clone = ae_obj_clone(list);
  
  puts("\nPrinting clone.");

  if (clone->type == AE_CONS____ && clone->head)
    ae_obj_each(clone, describe);

  puts("Done printing clone.\n");

  ae_obj_t * mapped = ae_obj_map(list, map_fun_clone);
  
  puts("\nPrinting mapped.");

  if (mapped->type == AE_CONS____ && mapped->head)
    ae_obj_each(mapped, describe);

  puts("Done printing mapped.");

  //return 0;

  pool_print();
}
