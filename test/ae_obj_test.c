#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

void
test_tutorial(void)
{
    ae_obj_t * obj = ALLOC_AE_OBJ;
    (void)obj;
}

TEST_LIST = {
    { "tutorial", test_tutorial },
    { NULL, NULL }
};
