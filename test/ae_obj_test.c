#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

#define FAIL TEST_CHECK(0)

#define SETUP_TEST                \
  pool_clear();                   \
  ae_obj_t * this = ALLOC_AE_OBJ; \
  ae_obj_t * that = ALLOC_AE_OBJ; \
  size_t counter = 1;             \
  (void)counter;                  \
  (void)that;

void newly_allocated_ae_obj_is_inside_pool(void)
{
  SETUP_TEST;
  TEST_CHECK(this >= pool_first && this <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", this, pool_first, pool_last);
}

void newly_initialized_ae_obj_has_correct_type_field(void) {
#define test(_type)                                                                                                                         \
  {                                                                                                                                         \
    SETUP_TEST;                                                                                                                             \
    ae_obj_init(this, _type);                                                                                                               \
    TEST_CHECK(this->type == _type);                                                                                                        \
    TEST_MSG("After ae_obj_init(obj, " #_type "), obj->type != " #_type ".");                                                               \
  }
  FOR_LEXED_TYPES_DO(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;
  ae_obj_init(this, AE_RATIONAL);
  TEST_CHECK(this->numerator_value == 0 && this->denominator_value == 0);
  TEST_MSG("After ae_obj_init(obj, AE_RATIONAL), its data fields should == 0.");
}

void cons(void) {
  SETUP_TEST;

  ae_obj_init(this, AE_INTEGER_);
  ae_obj_init(that, AE_INTEGER_);
  this->int_value = 1;
  this->int_value = 2;
  // CONS(this, that);
}

void unsafe_move(void) {
  SETUP_TEST;

  ae_obj_init(this, AE_CHAR____);
  this->char_value = 'x';

  ae_obj_init(that, AE_RATIONAL);
  that->numerator_value  = 123;
  that->denominator_value = 321;

  putchar('\n');
  printf("this %p and that is %p.\n", this, that);
  fputs("this ", stdout); ae_obj_put(this); putchar('\n');
  fputs("that ", stdout); ae_obj_put(that); putchar('\n');
  ae_obj_unsafe_move(this, that);
  fputs("this ", stdout); ae_obj_put(this); putchar('\n');
  fputs("that ", stdout); ae_obj_put(that); putchar('\n');

  TEST_CHECK(this->type == AE_RATIONAL);
  TEST_CHECK(this->numerator_value   == 123);
  TEST_CHECK(this->denominator_value == 321);

  TEST_CHECK(that->type == AE_FREE____);
  TEST_CHECK(that->numerator_value   == 0);
  TEST_CHECK(that->denominator_value == 0);
}

#define FOR_TEST_FUNS_DO(X)                                                                                                            \
  X(newly_allocated_ae_obj_is_inside_pool)                                                                                             \
  X(newly_initialized_ae_obj_has_correct_type_field)                                                                                   \
  X(newly_initialized_ae_obj_has_zeroed_data_fields)                                                                                   \
  X(unsafe_move)                                                                                                                       \
  X(cons)

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_TEST_FUNS_DO(pair)
  { NULL, NULL }
};
