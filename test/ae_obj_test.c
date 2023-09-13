#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

void test_newly_allocated_obj_is_inside_pool(void)
{
  ae_obj_t * obj = ALLOC_AE_OBJ;
  
  TEST_CHECK(obj >= pool_first && obj <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", obj, pool_first, pool_last);
}

TEST_LIST = {
  { "tutorial", test_newly_allocated_obj_is_inside_pool },
  { NULL, NULL }
};
