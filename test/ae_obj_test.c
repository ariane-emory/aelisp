#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

void
test_tutorial(void)
{
    ae_obj_t * obj = ALLOC_AE_OBJ;
    (void)obj;
    
    pool_print();
}

TEST_LIST = {
    { "tutorial", test_tutorial },
    { NULL, NULL }
};
