#include <stdio.h>
#include <stdlib.h>

#include "ae.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

int main() {
  ae_list_t list = 0; // malloc(sizeof(ae_list_t));
  ae_list_init(&list);

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = zero;
    printf("%s\n", ae_object_str(obj));
    ae_list_push_back(&list, obj);
  }
  
  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = one;
    printf("%s\n", ae_object_str(obj));
    ae_list_push_back(&list, obj);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = two;
    printf("%s\n", ae_object_str(obj));
    ae_list_push_back(&list, obj);
  }

  {
    ae_object_t * obj = malloc(sizeof(ae_object_t));
    ae_object_init(obj);
    obj->c_str = three;
    printf("%s\n", ae_object_str(obj));
    ae_list_push_back(&list, obj);
  }

  ae_list_node_t * position = list;
  
  for (; position; position = position->tail) {
    //printf("%s\n", ae_position_node_str(position));
    printf("%s\n", ae_object_str(position->object));
  }
}
