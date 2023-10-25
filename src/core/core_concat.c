#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");
  int total_length = 1;

  // Measure the new string's required length.
  FOR_EACH(elem, args) {
    REQUIRE(env, args, NILP(elem) || STRINGP(elem) || SYMBOLP(elem));
    if (NILP(elem))
      continue;

    char * str = SYMBOLP(elem)
      ? SYM_VAL(elem)
      : STR_VAL(elem);
    
    int len = strlen(str);

    total_length += len;
  }

  if (log_core)
    SLOGF("Expect length  %d.\n", total_length);
  
  // Allocate it.
  char * const string = free_list_malloc(total_length);
  
  int pos = 0;

  //Copy the argument strings into it.
  FOR_EACH(elem, args) {
    if (NILP(elem))
      continue;

    char * str = SYMBOLP(elem)
      ? SYM_VAL(elem)
      : STR_VAL(elem);

    int len = strlen(str);
    
    strcpy(string + pos, str);

    pos += len;
  }

  // Terminate it.
  string[total_length - 1] = '\0';
  
  CORE_RETURN("concat", NEW_STRING(string));
}
