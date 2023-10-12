#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");
  int total_length = 1;

  // Measure the new string's requred length.
  FOR_EACH(elem, args) {
    REQUIRE(env, args, STRINGP(elem));
    total_length += strlen(STR_VAL(elem));
  }

#ifdef AE_LOG_CORE
  SLOGF("Expect length  %d.\n", total_length);
#endif
  
  // Allocate it.
  char * const string = free_list_malloc(total_length);
  
  int pos = 0;

  //Copy the argument strings into it.
  FOR_EACH(elem, args) {
    int len = strlen(STR_VAL(elem));    
    strcpy(string + pos, STR_VAL(elem));
    pos += len;
  }

  // Terminate i.
  string[total_length - 1] = '\0';
  
  CORE_RETURN("concat", NEW_STRING(string));
}
