#include "core_includes.h"

void parse_line(const char * const line);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _read
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(read) {
  REQUIRE(STRINGP(CAR(args)));

  parse_line(STR_VAL(CAR(args)));

  REQUIRE(! read_error, "Could not read");
  REQUIRE(NILP(CDDR(program)), "Muliple s-exps in input");

  RETURN(CADR(program));
  
  END_DEF_CORE_FUN(read);
}

