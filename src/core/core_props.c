#include "core_includes.h"

#include "capture.h"
#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _set_props
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(set_props) {
  ae_obj_t * const obj            = CAR(args);

  REQUIRE(NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");

  ae_obj_t * const new_props_list = CADR(args);
  PROPS(obj)                      = new_props_list;
  ret                             = new_props_list;

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _props
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(props) {
  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = prop_list;               

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _put_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(put_prop) {
  ae_obj_t * const obj           = CAR(args);

  REQUIRE(NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");
       
  ae_obj_t * const key           = CADR(args);
  ae_obj_t * const value         = CADDR(args);
  ae_obj_t * const prop_list     = PROPS(obj);
  ae_obj_t * const new_prop_list = PSET_INTERNAL(prop_list, key, value);
  PROPS(obj)                     = new_prop_list;
  ret                            = value;

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(get_prop) {
  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const key       = CADR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = PGET(prop_list, key);

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _remove_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(remove_prop) {
  ae_obj_t * const obj       = CAR(args);

  REQUIRE(NILP(GET_PROP("no-user-props", obj)), "users cannot alter properties on this object");
    
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

  END_DEF_CORE_FUN;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _has_prop
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_CORE_FUN(has_prop) {
  ae_obj_t * const obj       = CAR(args);
  ae_obj_t * const key       = CADR(args);
  ae_obj_t * const prop_list = PROPS(obj);
  ret                        = TRUTH(PHAS(prop_list, key));

  END_DEF_CORE_FUN;
}

