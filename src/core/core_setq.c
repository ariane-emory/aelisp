#include "core_includes.h"
#include "eval.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq!
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_setq(ae_obj_t * const env, ae_obj_t * const args, __attribute__((unused)) int args_length) {
  CORE_BEGIN("setq!");

  ae_obj_t * sym      = CAR(args);
  ae_obj_t * val_expr = args_length == 1
    ? NIL
    : CADR(args);

  REQUIRE(env, args, SYMBOLP(sym),                "sym is not a symbol");
  REQUIRE(env, args, ! KEYWORDP(sym),             "keyword symbols may not be set");
  REQUIRE(env, args, ! HAS_PROP("constant", sym), "constant symbols may not be set");

  if (log_core) {
    LOG(sym, "setting symbol");
    LOG(val_expr, "to value"); 
    LOG(val_expr, "evaluating 'value' argument");
  }

  INDENT;

  ret = RETURN_IF_ERRORP(EVAL(env, val_expr));

  OUTDENT;

  if (log_core)
    LOG(ret, "evaluated 'value' argument is");

  if ((LAMBDAP(ret) || MACROP(ret)) && ! HAS_PROP("last-bound-to", ret)) {
    assert( (! NILP(ret)) && (! TRUEP(ret)));

    assert(! NILP(sym));
    assert(! TRUEP(sym));
    
    PUT_PROP(sym, "last-bound-to", ret);
    
    if (log_core)
      LOG(PROPS(ret), "core setq! val's new properties");
  }

  ae_env_lookup_mode_t mode = SPECIAL_SYMP(sym) ? GLOBAL : NEAREST;

  ENV_SET_4(mode, env, sym, ret);

end:
  
  CORE_RETURN("setq!", ret);
}

