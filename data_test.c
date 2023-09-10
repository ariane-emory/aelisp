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
  printf("%s\n", ae_object_str(ae_object));
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
  ae_object_t * object = malloc(sizeof(ae_object_t));
  ae_object_init(object);
  object->type = AE_LIST;

  ae_list_init(&object->list_value);  

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = zero;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }
  
  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = one;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = two;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = three;
    // printf("%s\n", ae_object_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  ae_list_each(&object->list_value, print_ae_object_str);

  ae_object_t * clone = ae_object_clone(object);

  printf("\n");

  ae_list_each(&clone->list_value, print_ae_object_str);
  
  /* ae_list_t num_list = 0; */

  /* for (int ix = 3; ix < 10; ix++) { */
  /*   int * new_num = malloc(sizeof(int)); */
  /*   *new_num = ix; */
  /*   ae_list_push_back(&num_list, new_num); */
  /* } */

  /* ae_list_each(&num_list, print_int_p); */
  /* printf("\n"); */
  /* ae_list_t doubled_list = ae_list_map(&num_list, double_int_p); */
  /* ae_list_map_into_from(&doubled_list, &num_list, id_int_p); */
  /* ae_list_each(&doubled_list, print_int_p); */
                                             
  /* printf("Length: %d\n", ae_list_node_length(doubled_list)); */
  /* printf("Length: %d\n", ae_list_length(&doubled_list)); */
}
