#include <stdio.h>

#include "acutest.h"

void
test_tutorial(void)
{
    void* mem;

    mem = malloc(10);
    TEST_CHECK(mem != NULL);

    mem = realloc(mem, 20);
    TEST_CHECK(mem != NULL);

    free(mem);
}

TEST_LIST = {
    { "tutorial", test_tutorial },
    { NULL, NULL }
};
