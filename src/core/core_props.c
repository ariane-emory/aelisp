#include "core_includes.h"

#include "capture.h"
#include "env.h"

#define MAYBE_EVAL(o) ({ CAPTURE((o)); (SYMBOLP(CAPTURED) && (! ENV_BOUNDP(env, CAPTURED))) ? CAPTURED : EVAL(env, CAPTURED); })

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_set_props(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("props!");

  ae_obj_t * value         = RETURN_IF_ERRORP(EVAL(env, CAR(args))); // this could be unsafe if value is NIL, maybe?
  ae_obj_t * key           = RETURN_IF_ERRORP(EVAL(env, CADR(args)));
  ae_obj_t * obj           = RETURN_IF_ERRORP(MAYBE_EVAL(CADDR(args)));
  ae_obj_t * prop_list     = PROPS(obj);
  ae_obj_t * new_prop_list = KSET(prop_list, key, value);
  PROPS(obj)               = new_prop_list;
  ret                      = value;

end:
  
  CORE_RETURN("props!", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_props(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("props");

  ae_obj_t * obj       = RETURN_IF_ERRORP(MAYBE_EVAL(CAR(args)));
  ae_obj_t * prop_list = PROPS(obj);
  ret                  = prop_list;               

end:
  
  CORE_RETURN("props", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_put_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("put!");

  ae_obj_t * value         = RETURN_IF_ERRORP(EVAL(env, CAR(args))); // this could be unsafe if value is NIL, maybe?
  ae_obj_t * key           = RETURN_IF_ERRORP(EVAL(env, CADR(args)));
  ae_obj_t * obj           = RETURN_IF_ERRORP(MAYBE_EVAL(CADDR(args)));
  ae_obj_t * prop_list     = PROPS(obj);
  ae_obj_t * new_prop_list = KSET(prop_list, key, value);
  PROPS(obj)               = new_prop_list;
  ret                      = value;

end:
  
  CORE_RETURN("put!", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_get_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("get");

  ae_obj_t * key       = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ae_obj_t * obj       = RETURN_IF_ERRORP(MAYBE_EVAL(CADR(args))); 
  ae_obj_t * prop_list = PROPS(obj);
  ret                  = KGET(prop_list, key);

end:
  
  CORE_RETURN("get", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_has_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("has?");

  ae_obj_t * key       = RETURN_IF_ERRORP(EVAL(env, CAR(args)));
  ae_obj_t * obj       = RETURN_IF_ERRORP(MAYBE_EVAL(CADR(args)));
  ae_obj_t * prop_list = PROPS(obj);
  ret                  = TRUTH(KHAS(prop_list, key));

end:
  
  CORE_RETURN("has?", ret);
}

