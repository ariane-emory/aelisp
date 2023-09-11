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
  ae_obj_init(object);
  object->type = AE_LIST;

  ae_list_init(&object->list_value);  

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj);
    obj->c_str = zero;
    // printf("%s\n", ae_obj_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }
  
  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj);
    obj->c_str = one;
    // printf("%s\n", ae_obj_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj);
    obj->c_str = two;
    // printf("%s\n", ae_obj_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  {
    ae_obj_t * obj = malloc(sizeof(ae_obj_t));
    ae_obj_init(obj);
    obj->c_str = three;
    // printf("%s\n", ae_obj_str(obj));
    ae_list_node_t * new_tail = ae_list_push_back(&object->list_value, obj);
    // printf("Added node 0x%x containing 0x%x.\n", new_tail, new_tail->object);
  }

  ae_list_each(&object->list_value, print_ae_obj_str);

  ae_obj_t * clone = ae_obj_clone(object);

  printf("\nPrint clone\n");

  ae_list_each(&clone->list_value, print_ae_obj_str);
  
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
