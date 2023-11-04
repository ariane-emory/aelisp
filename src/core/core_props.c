#include "core_includes.h"

#include "capture.h"
#include "env.h"

// #define MAYBE_EVAL(o) ({ CAPTURE((o)); (SYMBOLP(CAPTURED) && (! ENV_BOUNDP(env, CAPTURED))) ? CAPTURED : EVAL(env, CAPTURED); })

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_set_props(ae_obj_t * const env,
                             ae_obj_t * const args,
                             __attribute__((unused)) int args_length) {
  CORE_BEGIN("props!");

  ae_obj_t * const obj            = CAR(args);

  REQUIRE(env, args, NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");

  ae_obj_t * const new_props_list = CADR(args);
  PROPS(obj)                      = new_props_list;
  ret                             = new_props_list;

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

  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = prop_list;               

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

  ae_obj_t * const obj           = CAR(args);

  REQUIRE(env, args, NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");
       
  ae_obj_t * const key           = CADR(args);
  ae_obj_t * const value         = CADDR(args);
  ae_obj_t * const prop_list     = PROPS(obj);
  ae_obj_t * const new_prop_list = PSET_INTERNAL(prop_list, key, value);
  PROPS(obj)                     = new_prop_list;
  ret                            = value;

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

  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const key       = CADR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = PGET(prop_list, key);

end:
  
  CORE_RETURN("get", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_remove_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("remove");

  ae_obj_t * const obj       = CAR(args);

  REQUIRE(env, args, NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");
    
  ae_obj_t * const key       = CADR(args);

  if (! PHAS(PROPS(obj), key)) {
    ret = NIL;
    goto end;
  }

  ret = PGET(PROPS(obj), key);
  
  if (LENGTH(PROPS(obj)) <= 2)
    PROPS(obj) = NIL;
  else
    PREMOVE_MUTATING(PROPS(obj), key);

end:
  
  CORE_RETURN("remove", ret);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_has_prop(ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("has?");

  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const key       = CADR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = TRUTH(PHAS(prop_list, key));

end:
  
  CORE_RETURN("has?", ret);
}

