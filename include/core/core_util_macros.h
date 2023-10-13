#pragma once

#include <string.h>

#include "list.h"
#include "alist.h"
#include "obj.h"
#include "free_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper for avoiding double evaluation of macro parameters
////////////////////////////////////////////////////////////////////////////////////////////////////

#define CAPTURE(o)                       ae_obj_t * tmp_##__LINE__ = (o)
#define CAPTURED                         tmp_##__LINE__

////////////////////////////////////////////////////////////////////////////////////////////////////

#define REQUIRE(env, args, cond, ...)                                                              \
  if (! (cond)) {                                                                                  \
    char * fmt      = ("" __VA_ARGS__)[0]                                                          \
      ? "%s:%d: \"Error in %s: require " #cond ", " __VA_ARGS__ "!\""                              \
      : "%s:%d: \"Error in %s: require " #cond "!\"";                                              \
                                                                                                   \
    char * msg_tmp      = free_list_malloc(256);                                                   \
    sprintf(msg_tmp, fmt, __FILE__, __LINE__, __func__);                                           \
                                                                                                   \
    char * msg          = free_list_malloc(strlen(msg_tmp) + 1);                                   \
    strcpy(msg, msg_tmp);                                                                          \
    free_list_free(msg_tmp);                                                                       \
                                                                                                   \
    ae_obj_t * err_data = NIL;                                                                     \
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
  if (log_core) {                                                                                  \
    LOG(env,  "[applying 'core_"  name "' in env]", tmp);                                          \
    INDENT;                                                                                        \
  }                                                                                                \
  free(tmp);                                                                                       \
})

////////////////////////////////////////////////////////////////////////////////////////////////////

#define CORE_RETURN(name, val)                                                                     \
({                                                                                                 \
  if (log_core)                                                                                    \
    OUTDENT;                                                                                       \
  CAPTURE((val));                                                                                  \
  if (log_core)                                                                                    \
    LOG_RETURN_WITH_TYPE("core_" name, CAPTURED);                                                  \
  return CAPTURED;                                                                                 \
})

////////////////////////////////////////////////////////////////////////////////////////////////////
