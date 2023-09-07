#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "ae.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

void print_ae_object_str(void * ae_object_p) {
  ae_object_t * ae_object = ae_object_p;
  // printf("Iterate object 0x%x.\n", ae_object);
  printf("%s\n", ae_object_str(ae_object));
}


int main() {
  ae_list_t list = 0;
  ae_list_init(&list);

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = zero;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&list, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }
  
  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = one;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&list, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = two;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&list, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = three;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&list, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  ae_list_node_each(list, print_ae_object_str);

  ae_list_t num_list = 0;
  ae_list_init(&num_list);

  for (int ix = 3; ix < 10; ix++) {
    int * new_num = malloc(sizeof(int));
    *new_num = ix;
    ae_list_push_back(&num_list, new_num);
  }
}
