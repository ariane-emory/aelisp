#pragma once

#include <string.h>

#include "list.h"
#include "alist.h"
#include "obj.h"

#define REQUIRE(env, args, cond, ...)                                                              \
  if (! (cond)) {                                                                                  \
    char * fmt      = ("" __VA_ARGS__)[0]                                                          \
      ? "%s:%d: \"Error in %s: require " #cond ", " __VA_ARGS__ "!\""                              \
      : "%s:%d: \"Error in %s: require " #cond "!\"";                                              \
    char * msg      = free_list_malloc(256);                                                       \
    sprintf(msg, fmt, __FILE__, __LINE__, __func__);                                               \
                                                                                                   \
    ae_obj_t * err_data = NIL;                                                                     \
                                                                                                   \
    ASET(err_data, KW("args"), args);                                                              \
    ASET(err_data, KW("env"),  env);                                                               \
                                                                                                   \
    return NEW_ERROR(err_data, msg);                                                               \
  }

