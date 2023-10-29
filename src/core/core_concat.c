#include "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _concat
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_concat(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("concat");
  int total_length = 0;

  // Print out all input strings for debugging purposes
  FOR_EACH(elem, args) {
    if (NILP(elem))
      continue;
    char * str = SYMBOLP(elem)
      ? SYM_VAL(elem)
      : STR_VAL(elem);
    //printf("Input: %s\n", str);  // Debugging: Print input strings
  }

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

  // Account for the null terminator
  total_length += 1;

  if (log_core)
    SLOGF("Expect length %d.\n", total_length);
  
  // Allocate and initialize memory.
  char * const string = free_list_malloc(total_length);
  memset(string, 0, total_length);  // Initialize memory
  
  int pos = 0;

  // Copy the argument strings into it.
  FOR_EACH(elem, args) {
    if (NILP(elem))
      continue;

    char * str = SYMBOLP(elem)
      ? SYM_VAL(elem)
      : STR_VAL(elem);

    int len = strlen(str);

    //printf("Before: %s (pos: %d, len: %d)\n", string, pos, len);  // Debugging: Print before copying
    memcpy(string + pos, str, len);
    //printf("After: %s\n", string);  // Debugging: Print after copying

    pos += len;
  }

  // No need to manually null-terminate because we initialized memory to zeros

  CORE_RETURN("concat", NEW_STRING(string));
}
