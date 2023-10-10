#include "core_includes.h"
#include "eval.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
#ifdef AE_LOG_CORE
  // No CORE_BEGIN!
  LOG(args, "[core_setq]");
  INDENT;
#endif

  ae_obj_t * sym = CAR(args);
  ae_obj_t * val = CADR(args);

  REQUIRE(env, args, SYMBOLP(sym));
  REQUIRE(env, args, ! KEYWORDP(sym), "keyword symbols are constant");
  REQUIRE(env, args, sym != NIL,      "nil is a constant symbol");
  REQUIRE(env, args, sym != TRUE,     "t is a constant symbol");

#ifdef AE_LOG_CORE
  LOG(sym, "setting symbol");
  LOG(val, "to value");
#endif

#ifdef AE_LOG_CORE
  LOG(val, "evaluating 'value' argument");
  INDENT;
#endif

  val = EVAL(env, val);

#ifdef AE_LOG_CORE
  OUTDENT;
  LOG(val, "evaluated 'value' argument is");
#endif

#ifdef AE_DEBUG_OBJ
  if (LAMBDAP(val) || MACROP(val)) {
    DSET(val, "last-bound-to", sym);

#  ifdef AE_LOG_CORE
    LOG(DOBJ(val), "core setq val's new debug data");
#  endif

  }
#endif

  ENV_SET(env, sym, val);

  CORE_RETURN("setq", val);
}

