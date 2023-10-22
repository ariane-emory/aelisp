#include "core_includes.h"
#include "env.h"
#include "util.h"

#define MAYBE_EVAL(o) ({ CAPTURE((o)); (SYMBOLP(CAPTURED) && (! ENV_BOUNDP(env, CAPTURED))) ? CAPTURED : EVAL(env, CAPTURED); })

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_props(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("props");

  ae_obj_t * obj       = MAYBE_EVAL(CAR(args));
  ae_obj_t * prop_list = PROPS(obj);
  
  CORE_RETURN("props", prop_list);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_set_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("aset");

  ae_obj_t * value         = EVAL(env, CAR(args)); // this could be unsafe if value is NIL, maybe?
  ae_obj_t * key           = EVAL(env, CADR(args));
  ae_obj_t * obj           = MAYBE_EVAL(CADDR(args));
  ae_obj_t * prop_list     = PROPS(obj);
  ae_obj_t * new_prop_list = KSET(prop_list, key, value);

  PROPS(obj)            = new_prop_list;

  CORE_RETURN("daset", new_prop_list);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_get_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("get");

  ae_obj_t * key       = EVAL(env, CAR(args));
  ae_obj_t * obj       = MAYBE_EVAL(CADR(args)); 
  ae_obj_t * prop_list = PROPS(obj);

  CORE_RETURN("get", KGET(prop_list, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_has_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("has");

  ae_obj_t * key       = EVAL(env, CAR(args));
  ae_obj_t * obj       = MAYBE_EVAL(CADR(args));
  ae_obj_t * prop_list = PROPS(obj);

  CORE_RETURN("has", TRUTH(KHAS(prop_list, key)));
}

