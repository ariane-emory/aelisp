#include "core_includes.h"

#include "env.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eq
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eq(__attribute__((unused)) ae_obj_t * const env,
                      ae_obj_t * const args,
                      __attribute__((unused)) int args_length) {
  CORE_ENTER("eq");

  FOR_EACH(tailarg, CDR(args))
    if (CAR(args) != tailarg)
      RETURN(NIL);

  RETURN(TRUE);
  
  CORE_EXIT("eq");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _eql
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_eql(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_ENTER("eql");

  FOR_EACH(tailarg, CDR(args))
    if (! EQL(CAR(args), tailarg))
      RETURN(NIL);

  RETURN(TRUE);
  
  CORE_EXIT("eql");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_ENTER("tailp");
  RETURN(TRUTH(TAILP(CAR(args))));
  
  CORE_EXIT("tailp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_ENTER("properp");
  REQUIRE(env, args, CONSP(CAR(args)));
  RETURN(TRUTH(PROPERP(CAR(args))));
  
  CORE_EXIT("properp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _boundp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_boundp(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_ENTER("boundp");
  REQUIRE(env, args, SYMBOLP(CAR(args)));
  RETURN(TRUTH(ENV_BOUNDP(env, CAR(args))));
  
  CORE_EXIT("boundp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _keywordp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_keywordp(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_ENTER("keywordp");
  REQUIRE(env, args, SYMBOLP(CAR(args)));
  RETURN(TRUTH(KEYWORDP(CAR(args))));
  
  CORE_EXIT("keywordp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _id
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_id(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_ENTER("id");
  RETURN(CAR(args));
  
  CORE_EXIT("id");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_ENTER("not");
  RETURN(TRUTH(NILP(CAR(args))));
  
  CORE_EXIT("not");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nilp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nilp(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("nil?");
  RETURN(TRUTH(NILP(CAR(args))));

  CORE_EXIT("nil?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _zerop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_zerop(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("zero?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) == 0));
  
  CORE_EXIT("zero?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _onep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_onep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("one?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) == 1));
  
  CORE_EXIT("one?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _positivep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_positivep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("positive?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) >= 0));

  CORE_EXIT("positive?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _negativep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_negativep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_ENTER("negative?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) < 0));
  
  CORE_EXIT("negative?");
}

