#include <stdio.h>
#include <stdlib.h>
#include "mylang-data.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

int main() {
  mylang_list_t * list = malloc(sizeof(mylang_list_t));
  mylang_list_init(list);

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = zero;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_append(list, obj);
  }
  
  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = one;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_append(list, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = two;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_append(list, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = three;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_append(list, obj);
  }

  mylang_list_item_t * position = list->items;
  
  for (; position; position = position->tail) {
    //printf("%s\n", mylang_position_item_str(position));
    printf("%s\n", mylang_object_str(position->object));
  }
}
