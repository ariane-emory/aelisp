#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

#define SETUP_TEST pool_clear(); ae_obj_t * this = ALLOC_AE_OBJ; size_t counter = 1; (void)counter;
#define FAIL TEST_CHECK(0)

void test_newly_allocated_ae_obj_is_inside_pool(void)
{
  SETUP_TEST;
  TEST_CHECK(this >= pool_first && this <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", this, pool_first, pool_last);
}

void test_newly_initialized_ae_obj_has_correct_type_field(void) {
#define test_init(_type)                                                                                                                    \
  {                                                                                                                                         \
    SETUP_TEST;                                                                                                                             \
    ae_obj_init(this, _type);                                                                                                               \
    TEST_CHECK(this->type == _type);                                                                                                        \
    TEST_MSG("After ae_obj_init(obj, " #_type "), obj->type != " #_type ".");                                                               \
  }
  FOR_LEXED_TYPES_DO(test_init);
}

#define FOR_TEST_FUNS_DO(X)                                                                                                                 \
  X(test_newly_allocated_ae_obj_is_inside_pool)                                                                                             \
  X(test_newly_initialized_ae_obj_has_correct_type_field)

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_TEST_FUNS_DO(pair)
  { NULL, NULL }
};
