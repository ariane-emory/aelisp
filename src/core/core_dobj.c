#include  "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_props(
  __attribute__((unused)) ae_obj_t * const env,
  __attribute__((unused)) ae_obj_t * const args,
  __attribute__((unused)) int              args_length) {
  CORE_BEGIN("props");

  CORE_RETURN("props", PROPS(CAR(args)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_set_prop(__attribute__((unused)) ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("aset");

  ae_obj_t * key           = CAR(args);
  ae_obj_t * value         = CADR(args); // this could be unsafe if value is NIL, maybe.
  ae_obj_t * obj           = CADDR(args);
  ae_obj_t * prop_list     = PROPS(obj);
  ae_obj_t * new_prop_list = KSET(prop_list, key, value);

  PROPS(obj)            = new_prop_list;

  CORE_RETURN("daset", new_prop_list);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_get_prop(__attribute__((unused)) ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("get");

  ae_obj_t * key       = CAR(args);
  ae_obj_t * obj       = CADR(args); 
  ae_obj_t * prop_list = PROPS(obj);

  CORE_RETURN("get", KGET(prop_list, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_has_prop(__attribute__((unused)) ae_obj_t * const env,
                            ae_obj_t * const args,
                            __attribute__((unused)) int args_length) {
  CORE_BEGIN("has");

  ae_obj_t * key       = CAR(args);
  ae_obj_t * obj       = CADR(args);
  ae_obj_t * prop_list = PROPS(obj);

  CORE_RETURN("has", TRUTH(KHAS(prop_list, key)));
}

