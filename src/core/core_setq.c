#include "core_includes.h"
#include "eval.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _setq
////////////////////////////////////////////////////////////////////////////////////////////////////
DEF_CORE_FUN(setq) {
  REQUIRE(!(args_length % 2), "setq requires an even number of arguments");
  
  ae_obj_t * pair = args;

  while (! NILP(pair) && ! NILP(CDR(pair))) {
    ae_obj_t * const sym      = CAR(pair);
    ae_obj_t * const val_expr = CADR(pair);

    REQUIRE(SYMBOLP(sym),                    "sym is not a symbol");
    REQUIRE(!KEYWORDP(sym),                  "keyword symbols may not be set");
    REQUIRE(NILP(GET_PROP("constant", sym)), "constant symbols may not be set");

    if (log_core) {
      LOG(sym,      "setting symbol");
      LOG(val_expr, "to value");
    }

    INDENT;
    ret = RETURN_IF_ERRORP(EVAL(env, val_expr));
    OUTDENT;

    if (log_core) LOG(ret, "evaluated 'value' argument is");

    if ((LAMBDAP(ret) || MACROP(ret)) && ! HAS_PROP("last-bound-to", ret)) {
      assert( (! NILP(ret)) && (! TRUEP(ret)));

      PUT_PROP(sym, "last-bound-to", ret);
    
      if (log_core)
        LOG(PROPS(ret), "core setq! val's new properties");
    }

    ae_env_lookup_mode_t mode = SPECIAL_SYMP(sym) ? GLOBAL : NEAREST;

    ENV_SET_4(mode, env, sym, ret);

    pair = CDDR(pair);
  }

  END_DEF_CORE_FUN;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
