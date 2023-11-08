#pragma once

#include <string.h>
#include <stdio.h>

#include "capture.h"
#include "common.h"
#include "free_list.h"
#include "jump_return.h"
#include "log.h"
#include "obj.h"
#include "plist.h"
#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
#define REQUIRE(env, args, cond, ...)                                                              \
  if (! (cond)) {                                                                                  \
    char * const fmt      = ("" __VA_ARGS__)[0]                                                    \
      ? "%s:%d: \"Error in %s: require " #cond ", " __VA_ARGS__ "!\""                              \
      : "%s:%d: \"Error in %s: require " #cond "!\"";                                              \
                                                                                                   \
    ae_obj_t * const err = NEW_ERROR(fmt, __FILE__, __LINE__, __func__);                           \
                                                                                                   \
    PUT_PROP(args, "error-args", err);                                                             \
    PUT_PROP(env,  "error-env",  err);                                                             \
                                                                                                   \
    return err;                                                                                    \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_ENTER(name)                                                                           \
  JUMP_RETURN_ENTER;                                                                               \
                                                                                                   \
  assert(env);                                                                                     \
  assert(ENVP(env));                                                                               \
  assert(args);                                                                                    \
  assert(TAILP(args));                                                                             \
  assert(args_length >= 0);                                                                        \
                                                                                                   \
  {                                                                                                \
    char * tmp##__LINE__ = SWRITE(env);                                                            \
    if (log_core)                                                                                  \
      LOG(env,  "[applying 'core_"  name "' in env]", tmp##__LINE__);                              \
    INDENT;                                                                                        \
    free(tmp##__LINE__);                                                                           \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_EXIT(name)                                                                          \
  {                                                                                                \
    if (local_indents) OUTDENT;                                                                    \
    if (log_core)                                                                                  \
      LOG_RETURN_WITH_TYPE("core_" name, ret);                                                     \
    return ret;                                                                                    \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
