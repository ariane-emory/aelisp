#pragma once

#include <string.h>

#include "common.h"
#include "obj.h"
#include "free_list.h"
#include "log.h"
#include "util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define REQUIRE(env, args, cond, ...)                                                              \
  if (! (cond)) {                                                                                  \
    char * fmt      = ("" __VA_ARGS__)[0]                                                          \
      ? "%s:%d: \"Error in %s: require " #cond ", " __VA_ARGS__ "!\""                              \
      : "%s:%d: \"Error in %s: require " #cond "!\"";                                              \
                                                                                                   \
    char * const msg_tmp = free_list_malloc(256);                                                  \
    snprintf(msg_tmp, 256, fmt, __FILE__, __LINE__, __func__);                                     \
                                                                                                   \
    char * const msg = free_list_malloc(strlen(msg_tmp) + 1);                                      \
    strcpy(msg, msg_tmp);                                                                          \
    free_list_free(msg_tmp);                                                                       \
                                                                                                   \
    ae_obj_t * const err_data = NIL;                                                               \
                                                                                                   \
    KSET(err_data, KW("args"), args);                                                              \
    KSET(err_data, KW("env"),  env);                                                               \
                                                                                                   \
    return NEW_ERROR(msg, err_data);                                                               \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_BEGIN(name)                                                                           \
({                                                                                                 \
  char * tmp = SWRITE(env);                                                                        \
  if (log_core)                                                                                    \
    LOG(env,  "[applying 'core_"  name "' in env]", tmp);                                          \
  INDENT;                                                                                          \
  free(tmp);                                                                                       \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_RETURN(name, val)                                                                     \
({                                                                                                 \
  OUTDENT;                                                                                         \
  CAPTURE((val));                                                                                  \
  if (log_core)                                                                                    \
    LOG_RETURN_WITH_TYPE("core_" name, CAPTURED);                                                  \
  return CAPTURED;                                                                                 \
})
////////////////////////////////////////////////////////////////////////////////////////////////////
