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
      RETURN(NIL);

  RETURN(TRUE);
  
end:
  
  CORE_RETURN("eq");
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
      RETURN(NIL);

  RETURN(TRUE);
  
end:
  
  CORE_RETURN("eql");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _tailp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_tailp(ae_obj_t * const env,
                         ae_obj_t * const args,
                         __attribute__((unused)) int args_length) {
  CORE_BEGIN("tailp");
  RETURN(TRUTH(TAILP(CAR(args))));
  
end:
  
  CORE_RETURN("tailp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _properp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_properp(ae_obj_t * const env,
                           ae_obj_t * const args,
                           __attribute__((unused)) int args_length) {
  CORE_BEGIN("properp");
  REQUIRE(env, args, CONSP(CAR(args)));
  RETURN(TRUTH(PROPERP(CAR(args))));
  
end:
  
  CORE_RETURN("properp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _boundp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_boundp(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("boundp");
  REQUIRE(env, args, SYMBOLP(CAR(args)));
  RETURN(TRUTH(ENV_BOUNDP(env, CAR(args))));
  
end:
  
  CORE_RETURN("boundp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _keywordp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_keywordp(ae_obj_t * const env,
                          ae_obj_t * const args,
                          __attribute__((unused)) int args_length) {
  CORE_BEGIN("keywordp");
  REQUIRE(env, args, SYMBOLP(CAR(args)));
  RETURN(TRUTH(KEYWORDP(CAR(args))));
  
end:
  
  CORE_RETURN("keywordp");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _id
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_id(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("id");
  RETURN(CAR(args));
  
end:
  
  CORE_RETURN("id");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _not
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_not(__attribute__((unused)) ae_obj_t * const env,
                       ae_obj_t * const args,
                       __attribute__((unused)) int args_length) {
  CORE_BEGIN("not");
  RETURN(TRUTH(NILP(CAR(args))));
  
end:
  
  CORE_RETURN("not");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _nilp
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_nilp(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("nil?");
  RETURN(TRUTH(NILP(CAR(args))));

end:
  
  CORE_RETURN("nil?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _zerop
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_zerop(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("zero?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) == 0));
  
end:
  
  CORE_RETURN("zero?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _onep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_onep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("one?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) == 1));
  
end:
  
  CORE_RETURN("one?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _positivep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_positivep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("positive?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) >= 0));

end:
  
  CORE_RETURN("positive?");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _negativep
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * ae_core_negativep(__attribute__((unused)) ae_obj_t * const env,
                        ae_obj_t * const args,
                        __attribute__((unused)) int args_length) {
  CORE_BEGIN("negative?");
  REQUIRE(env, args, INTEGERP(CAR(args)), "argument must be an integer");
  RETURN(TRUTH(INT_VAL(CAR(args)) < 0));
  
end:
    
  CORE_RETURN("negative?");
}

