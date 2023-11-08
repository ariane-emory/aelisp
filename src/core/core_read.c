#include "core_includes.h"

void parse_line(const char * const line);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _read
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(read) {
  REQUIRE(env, args, STRINGP(CAR(args)));

  parse_line(STR_VAL(CAR(args)));

  REQUIRE(env, args, ! read_error, "Could not read");
  REQUIRE(env, args, NILP(CDDR(program)), "Muliple s-exps in input");

  RETURN(CADR(program));
  
  END_DEF_CORE_FUN(read);
}

