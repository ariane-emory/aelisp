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
  ae_obj_t * list = malloc(sizeof(ae_obj_t));
  ae_obj_init(list, AE_LIST);

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = zero;
    fputs("Pushing ", stdout);
    ae_obj_put(obj);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj);
  }
  
  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = one;
    fputs("Pushing ", stdout);
    ae_obj_put(obj);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = two;
    fputs("Pushing ", stdout);
    ae_obj_put(obj);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = three;
    fputs("Pushing ", stdout);
    ae_obj_put(obj);
    fputs(" into ", stdout);
    ae_obj_put(list);
    putchar('\n');
    ae_obj_push_back(list, obj);
  }

  fputs("Done pushing into ", stdout);
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
