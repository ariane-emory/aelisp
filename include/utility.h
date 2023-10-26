#pragma once

#include <string.h>

#include "obj.h"
#include "free_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define MAKE_ERROR(...)                                                                            \
  ({                                                                                               \
    char * const tmp = free_list_malloc(256);                                                      \
    snprintf(tmp, 256, __VA_ARGS__);                                                               \
    char * const err_msg = free_list_malloc(strlen(tmp) + 1);                                      \
    strcpy(err_msg, tmp);                                                                          \
    free_list_free(tmp);                                                                           \
    NEW_ERROR(err_msg);                                                                            \
  })

