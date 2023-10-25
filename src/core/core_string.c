#include <string.h>

#include "core_includes.h"

#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _string
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_string(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("string");

  char * const tmp  = SPRINC(CAR(args));
  char * const tmp2 = free_list_malloc(strlen(tmp) + 1);
  strcpy(tmp2, tmp);

  ret = NEW_STRING(tmp2);
  
  CORE_RETURN("string", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _symbol
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_symbol(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("symbol");

  if (! STRINGP(CAR(args)))
    LOG(CAR(args), "not a string");
  
  REQUIRE(env, args, STRINGP(CAR(args)), "symbol's 1st arg must be a string");
  ret = SYM(STR_VAL(CAR(args)));
  
  CORE_RETURN("symbol", ret);
}

