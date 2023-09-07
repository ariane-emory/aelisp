#include <stdio.h>
#include <stdlib.h>
#include "mylang-data.h"

static char * zero  = "Zero";
static char * one   = "One";
static char * two   = "Two";
static char * three = "Three";

int main() {
  mylang_list_item_t * head = malloc(sizeof(mylang_list_item_t));
  mylang_list_item_init(head);

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = zero;
    printf("%s\n", mylang_object_str(obj));
    
    head->data = obj;
  }
  
  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = one;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_item_append(head, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = two;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_item_append(head, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = three;
    printf("%s\n", mylang_object_str(obj));
    
    mylang_list_item_append(head, obj);
  }

  for (; head; head = head->tail) {
    //printf("%s\n", mylang_list_item_str(head));
    printf("%s\n", mylang_object_str(head->data));
  }
}
