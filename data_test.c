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

void print_int_p(ae_obj_t * int_p) {
  printf("%d\n",*(int*)int_p);
}

int main() {
  putchar('\n');

  ae_obj_t * list = malloc(sizeof(ae_obj_t));
  ae_obj_init(list, AE_LIST);

  {
    ae_obj_t * obj_one = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj_one, AE_STRING);
    obj_one->str_value = zero;
    fputs("Pushing ", stdout);
    ae_obj_put(obj_one);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj_one);
  }
  
  {
    ae_obj_t * obj_two = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj_two, AE_STRING);
    obj_two->str_value = one;
    fputs("Pushing ", stdout);
    ae_obj_put(obj_two);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj_two);
  }

  {
    ae_obj_t * obj_three = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj_three, AE_STRING);
    obj_three->str_value = two;
    fputs("Pushing ", stdout);
    ae_obj_put(obj_three);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj_three);
  }

  {
    ae_obj_t * obj_four = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj_four, AE_STRING);
    obj_four->str_value = three;
    fputs("Pushing ", stdout);
    ae_obj_put(obj_four);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj_four);
  }

  fputs("\nDone pushing into ", stdout);
  ae_obj_put(list);
  putchar('\n');
  putchar('\n');
    
  if (list->type == AE_LIST && list->head)
    ae_obj_each(list, describe);

  ae_obj_t * clone = ae_obj_clone(list);

  printf("\nPrint clone\n");

  if (clone->type == AE_LIST && clone->head)
    ae_obj_each(clone, describe);
}
