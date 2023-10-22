#include "core_includes.h"
#include "eval.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq!
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  if (log_core)
    // No CORE_BEGIN!
    LOG(args, "[core_setq!]");

  INDENT;

  ae_obj_t * sym = CAR(args);
  ae_obj_t * val = args_length == 1
    ? NIL
    : CADR(args);

  REQUIRE(env, args, SYMBOLP(sym));
  REQUIRE(env, args, ! HAS_PROP("constant",  sym), "constant symbols cannot be set");
  REQUIRE(env, args, ! KEYWORDP(sym), "keyword symbols are constant and cannot be set");
  REQUIRE(env, args, sym != NIL,      "nil is a constant symbol and cannot be set");
  REQUIRE(env, args, sym != TRUE,     "t is a constant symbol and cannot be set");

  if (log_core) {
    LOG(sym, "setting symbol");
    LOG(val, "to value"); 
    LOG(val, "evaluating 'value' argument");
  }

  INDENT;

  val = EVAL(env, val);

  OUTDENT;

  if (log_core)
    LOG(val, "evaluated 'value' argument is");

  if (LAMBDAP(val) || MACROP(val)) {
    PUT_PROP(val, "last-bound-to", sym);

    if (log_core)
      LOG(PROPS(val), "core setq! val's new properties");
  }

  ENV_SET(env, sym, val);

  CORE_RETURN("setq!", val);
}

