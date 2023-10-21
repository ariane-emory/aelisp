#include  "core_includes.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_props(
  __attribute__((unused)) ae_obj_t * const env,
  __attribute__((unused)) ae_obj_t * const args,
  __attribute__((unused)) int              args_length) {
  CORE_BEGIN("props");

#ifdef AE_DEBUG_OBJ
  CORE_RETURN("props", DOBJ(CAR(args)));
#else
  CORE_RETURN("props", NIL);
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _daset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_daset(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("aset");

  ae_obj_t * key           = CAR(args);
  ae_obj_t * value         = CADR(args); // this could be unsafe if value is NIL, maybe.
  ae_obj_t * obj           = CADDR(args);
  ae_obj_t * prop_list     = DOBJ(obj);
  ae_obj_t * new_prop_list = KSET(prop_list, key, value);

  DOBJ(obj)            = new_prop_list;

  CORE_RETURN("daset", new_prop_list);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_get(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("get");

  ae_obj_t * key       = CAR(args);
  ae_obj_t * obj       = CADR(args); 
  ae_obj_t * prop_list = DOBJ(obj);

  CORE_RETURN("get", KGET(prop_list, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_has(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("has");

  ae_obj_t * key       = CAR(args);
  ae_obj_t * obj       = CADR(args);
  ae_obj_t * prop_list = DOBJ(obj);

  CORE_RETURN("has", TRUTH(KHAS(prop_list, key)));
}

