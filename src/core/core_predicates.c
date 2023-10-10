#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(__attribute__((unused)) ae_obj_t * const env,
                      ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_BEGIN("eq");

  FOR_EACH(tailarg, CDR(args))
    if (CAR(args) != tailarg)
      CORE_RETURN("eq", NIL);

  CORE_RETURN("eq", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("eql");

  FOR_EACH(tailarg, CDR(args))
    if (! EQL(CAR(args), tailarg))
      CORE_RETURN("eql", NIL);

  CORE_RETURN("eql", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("tailp");
  REQUIRE(env, args, TAILP(CAR(args)));
  CORE_RETURN("tailp", TAILP(CAR(args)) ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("properp");
  REQUIRE(env, args, CONSP(CAR(args)));
  CORE_RETURN("properp", PROPERP(CAR(args)) ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _boundp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_boundp(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("boundp");
  REQUIRE(env, args, SYMBOLP(CAR(args)));
  CORE_RETURN("boundp", ENV_BOUNDP(env, CAR(args)) ? TRUE : NIL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("not");

  FOR_EACH(elem, args)
    if (! NILP(elem))
      CORE_RETURN("not", NIL);

  CORE_RETURN("not", TRUE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nilp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nilp(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("nilp");
  CORE_RETURN("nilp", NILP(CAR(args)) ? TRUE : NIL);
}

