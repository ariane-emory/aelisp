#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");
  int total_length = 1;
  
  FOR_EACH(elem, args) {
    REQUIRE(env, args, STRINGP(elem));
    total_length += strlen(STR_VAL(elem));
  }

  SLOGF("Expect %d.\n", total_length);

  char * const string = free_list_malloc(total_length);
  memset(string, 0, total_length);
  
  int pos = 0;
  
  FOR_EACH(elem, args) {
    // printf("'%s'\n", string);
    
    int len = strlen(STR_VAL(elem));
    
    strcpy(string + pos, STR_VAL(elem));
    pos += len;
  }

  string[total_length - 1] = '\0';
  
  CORE_RETURN("concat", NEW_STRING(string));
}
