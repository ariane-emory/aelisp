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
#define REQUIRE(cond, ...)                                                                         \
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
#define CORE_BEGIN(name)                                                                           \
  JUMP_RETURN_ENTER;                                                                               \
                                                                                                   \
  assert(env);                                                                                     \
  assert(ENVP(env));                                                                               \
  assert(args);                                                                                    \
  assert(TAILP(args));                                                                             \
  assert(args_length >= 0);                                                                        \
                                                                                                   \
  const char * const core_fun_name = name;                                                         \
                                                                                                   \
  {                                                                                                \
    char * tmp##__LINE__ = SWRITE(env);                                                            \
    if (log_core)                                                                                  \
      LOG(env,  "[applying 'core_"  name "' in env]", tmp##__LINE__);                              \
    INDENT;                                                                                        \
    free(tmp##__LINE__);                                                                           \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define DEF_CORE_FUN(name)                                                                         \
  ae_obj_t * ae_core_##name(__attribute__((unused)) ae_obj_t * const env,                          \
                            __attribute__((unused)) ae_obj_t * const args,                         \
                            __attribute__((unused)) int              args_length) {                \
                                                                                                   \
  CORE_BEGIN(#name);                                                                             
////////////////////////////////////////////////////////////////////////////////////////////////////
#define END_DEF_CORE_FUN                                                                           \
  }                                                                                                \
                                                                                                   \
end:                                                                                               \
                                                                                                   \
if (local_indents)                                                                                 \
  OUTDENT;                                                                                         \
                                                                                                   \
if (log_core) {                                                                                    \
  char * const tmp##__LINE__ = free_list_malloc(256);                                              \
  snprintf(tmp##__LINE__, 256, "returning from 'core_%s'", core_fun_name);                         \
                                                                                                   \
  LOG_RETURN_WITH_TYPE(tmp##__LINE__, ret);                                                        \
                                                                                                   \
  free_list_free(tmp##__LINE__);                                                                   \
}                                                                                                  \
                                                                                                   \
return ret;
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CORE_END(name)                                                                             \
  end:                                                                                             \
                                                                                                   \
  if (local_indents)                                                                               \
    OUTDENT;                                                                                       \
                                                                                                   \
  if (log_core)                                                                                    \
    LOG_RETURN_WITH_TYPE("core_" name, ret);                                                       \
                                                                                                   \
  return ret;
////////////////////////////////////////////////////////////////////////////////////////////////////
#define UNTIL_NILP(expr)     while (! NILP((expr)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define UNTIL_NOT_NILP(expr) while (NILP((expr)))
////////////////////////////////////////////////////////////////////////////////////////////////////
