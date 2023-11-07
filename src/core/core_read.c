#include "core_includes.h"

void parse_line(const char * const line);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _read
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_read(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("read");
  
  REQUIRE(env, args, STRINGP(CAR(args)));

  parse_line(STR_VAL(CAR(args)));

  REQUIRE(env, args, ! read_error, "Could not read");
  REQUIRE(env, args, NILP(CDDR(program)), "Muliple s-exps in input");
  
  CORE_RETURN("read", CADR(program));
}

