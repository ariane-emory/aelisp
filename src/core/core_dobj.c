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

  ae_obj_t * key       = CAR(args);
  ae_obj_t * value     = CADR(args); // this could be unsafe if value is NIL, maybe.
  ae_obj_t * obj       = CADDR(args);
  
  ae_obj_t * alist     = DOBJ(obj);
  ae_obj_t * new_alist = ASET(alist, key, value);

  DOBJ(obj)            = new_alist;

  CORE_RETURN("daset", new_alist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _daget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_daget(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("daget");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * obj   = CADR(args);
  
  ae_obj_t * alist = DOBJ(obj);

  CORE_RETURN("daget", AGET(alist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dahas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dahas(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("dahas");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * obj   = CADR(args);
  
  ae_obj_t * alist = DOBJ(obj);

  CORE_RETURN("dahas", TRUTH(AHAS(alist, key)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dpset
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dpset(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("dpset");

  ae_obj_t * key       = CAR(args);
  ae_obj_t * value     = CADR(args); // this could be unsafe if value is NIL, maybe.
  ae_obj_t * obj       = CADDR(args);
  
  ae_obj_t * plist     = DOBJ(obj);
  ae_obj_t * new_plist = PSET(plist, key, value);

  DOBJ(obj)            = new_plist;
  
  CORE_RETURN("dpset", new_plist);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dpget
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dpget(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("dpget");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * obj   = CADR(args);
  
  ae_obj_t * plist = DOBJ(obj);

  CORE_RETURN("dpget", PGET(plist, key));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dphas
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_dphas(__attribute__((unused)) ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("dphas");

  ae_obj_t * key   = CAR(args);
  ae_obj_t * obj   = CADR(args);
  
  ae_obj_t * plist = DOBJ(obj);

  CORE_RETURN("dphas", TRUTH(PHAS(plist, key)));
}

