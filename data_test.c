#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae_list.h"
#include "ae_obj.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

void describe(void * ae_obj_p) {
  ae_obj_t * ae_obj = ae_obj_p;
  ae_obj_put((ae_obj_t *)ae_obj_p);
  putchar('\n');
}

void print_int_p(void * int_p) {
  printf("%d\n",*(int*)int_p);
}

int main() {
  ae_obj_t * object = malloc(sizeof(ae_obj_t));
  ae_obj_init(object, AE_LIST);

  ae_list_init(&object->list_value);  

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = zero;
    ae_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }
  
  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = one;
    ae_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = two;
    ae_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_STRING);
    obj->str_value = three;
    ae_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  ae_obj_list_each(&object->list_value, describe);

  ae_obj_t * clone = ae_obj_clone(object);

  printf("\nPrint clone\n");

  ae_obj_list_each(&clone->list_value, describe);
}
