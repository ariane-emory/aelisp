#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae_list.h"
#include "ae_obj.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

void print_ae_obj_str(void * ae_obj_p) {
  ae_obj_t * ae_obj = ae_obj_p;
  printf("%s\n", ae_obj_str(ae_obj));
}

void print_int_p(void * int_p) {
  printf("%d\n",*(int*)int_p);
}

void * double_int_p(void * int_p) {
  int * new_int = malloc(sizeof(int));
  *new_int = *(int*)int_p * 2;
  return new_int;
}

void * id_int_p(void * int_p) {
  int tmp  = *(int*)int_p;
  int * new_int = malloc(sizeof(int));
  *new_int = tmp;
  return new_int;
}

int main() {
  ae_obj_t * object = malloc(sizeof(ae_obj_t));
  ae_obj_init(object, AE_LIST);

  ae_list_init(&object->list_value);  

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_INTEGER);
    obj->c_str = zero;
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }
  
  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_INTEGER);
    obj->c_str = one;
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_INTEGER);
    obj->c_str = two;
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj, AE_INTEGER);
    obj->c_str = three;
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
  }

  ae_list_each(&object->list_value, print_ae_obj_str);

  ae_obj_t * clone = ae_obj_clone(object);

  printf("\nPrint clone\n");

  ae_list_each(&clone->list_value, print_ae_obj_str);
}
