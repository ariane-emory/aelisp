#include "core_includes.h"


////////////////////////////////////////////////////////////////////////////////////////////////////
// _macro
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(macro) {
  REQUIRE(TAILP(CAR(args)) || SYMBOLP(CAR(args)));
  REQUIRE(TAILP(CDR(args)));

  // symbols used in params must not be '*special*':
  if (SYMBOLP(CAR(args))) {
      REQUIRE(! SPECIAL_SYMP(CAR(args)), "special symbols cannot be used as params");
  }
  else {
    FOR_EACH (param, CAR(args)) {
      REQUIRE(SYMBOLP(param));
      REQUIRE(! SPECIAL_SYMP(param), "special symbols cannot be used as params");
    }
  }

  RETURN(NEW_MACRO(CAR(args), CDR(args), env));
  
  END_DEF_CORE_FUN(macro);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _lambda
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(lambda) {
  REQUIRE(TAILP(CAR(args)) || SYMBOLP(CAR(args)));
  REQUIRE(TAILP(CDR(args)));

  // symbols used in params must not be '*special*':
  if (SYMBOLP(CAR(args))) {
      REQUIRE(! SPECIAL_SYMP(CAR(args)), "special symbols cannot be used as params");
  }
  else {
    FOR_EACH (param, CAR(args)) {
      REQUIRE(SYMBOLP(param));
      REQUIRE(! SPECIAL_SYMP(param), "special symbols cannot be used as params");
    }
  }

  RETURN(NEW_LAMBDA(CAR(args), CDR(args), env));
  
  END_DEF_CORE_FUN(lambda);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _params
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(params) {
  REQUIRE(MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_PARAMS(CAR(args)));
  
  END_DEF_CORE_FUN(params);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _body
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(body) {
  REQUIRE(MACROP(CAR(args)) || LAMBDAP(CAR(args)));

  RETURN(FUN_BODY(CAR(args)));
  
  END_DEF_CORE_FUN(body);
}

