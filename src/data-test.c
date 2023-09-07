#include <stdlib.h>
#include "mylang-data.h"

static char * one = "One";
static char * two = "Two";
static char * three = "Three";

int main() {
  mylang_list_t * hed = malloc(sizeof(mylang_list_t));
  mylang_list_init(hed);

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = one;
    
    mylang_list_append(hed, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = two;
    
    mylang_list_append(hed, obj);
  }

  {
    mylang_object_t * obj = malloc(sizeof(mylang_object_t));
    mylang_object_init(obj);
    obj->c_str = three;
    
    mylang_list_append(hed, obj);
  }
}
