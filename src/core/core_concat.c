#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");

  int total_length = 0;

  FOR_EACH(elem, args) {
    REQUIRE(env, args, NILP(elem) || STRINGP(elem));
    
    if (NILP(elem))
      continue;

    char * str = STR_VAL(elem);
    int    len = strlen(str);
    
    total_length += len;
  }

  // Account for the null terminator
  total_length += 1;

  if (log_core)
    SLOGF("Expect length %d.\n", total_length);
  
  char * const string = free_list_malloc(total_length);
  memset(string, 0, total_length);
  
  int pos = 0;

  FOR_EACH(elem, args) {
    if (NILP(elem))
      continue;

    char * str = STR_VAL(elem);
    int    len = strlen(str);

    memcpy(string + pos, str, len);

    pos += len;
  }

  // No need to manually null-terminate because we initialized memory to zeros

  RETURN(NEW_STRING(string));
  
  CORE_END("concat");
}
