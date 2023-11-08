#include <string.h>

#include "core_includes.h"

#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _string
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_string(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("string");

  char * const tmp  = SPRINC(CAR(args));
  char * const tmp2 = free_list_malloc(strlen(tmp) + 1);
  strcpy(tmp2, tmp);

  RETURN(NEW_STRING(tmp2));

end:
  
  CORE_EXIT("string");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _intern
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_intern(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_ENTER("intern");

  if (! STRINGP(CAR(args)))
    LOG(CAR(args), "not a string");
  
  REQUIRE(env, args, STRINGP(CAR(args)), "intern's 1st arg must be a string");

  RETURN(SYM(STR_VAL(CAR(args))));

end:
  
  CORE_EXIT("intern");
}

