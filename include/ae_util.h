#pragma once

#include <stdio.h>

#include "ae_obj.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FF               (fflush(stdout))
#define NL               (putchar('\n'))
#define SPC              (putchar(' '))

#ifdef AE_DEADLY_MARGINJ
#  define INDENT         { PR("IN  AT %s:%d!", __FILE__,  __LINE__); (indent ()); }
#  define OUTDENT        { PR("OUT AT %s:%d!", __FILE__,  __LINE__); (outdent()); }
#else
#  define INDENT         { (indent ()); }
#  define OUTDENT        { (outdent()); }
#endif

#define PR(...)          (fprintf(stdout,   __VA_ARGS__))
#define FPR(stream, ...) (fprintf((stream), __VA_ARGS__))
#define ARRAY_SIZE(a)    (sizeof(a) / sizeof((a)[0]))
#define OLOG(o)          (obj_log((o), (#o)))

#define LOG(o, ...)                                                                                 \
{                                                                                                   \
  sprintf(obj_log_buffer, __VA_ARGS__);                                                             \
  obj_log((o), (obj_log_buffer));                                                                   \
}

#define SLOG(s)                                                                                     \
{                                                                                                   \
  NL;                                                                                               \
  int written  = 0;                                                                                 \
  while (written ++ < (indentation << 1)) SPC;                                                      \
  (fprintf(stdout, (s)));                                                                           \
}

#define FLOG(s, ...)                                                                                \
{                                                                                                   \
  NL;                                                                                               \
  int written  = 0;                                                                                 \
  while (written ++ < (indentation << 1)) SPC;                                                      \
  (fprintf(stdout, (s), __VA_ARGS__));                                                              \
}

#define LOG_RETURN_WITH_TYPE(fun_name, val)                                                        \
({                                                                                                 \
    const char * type = GET_TYPE_STR(val);                                                         \
    /* */ char * tmp  = free_list_malloc(strlen(type) + 2);                                        \
    sprintf(tmp, ":%s", type);                                                                     \
                                                                                                   \
    LOG(val, "[%s returning a %s]", fun_name, tmp);                                                \
                                                                                                   \
    free_list_free(tmp);                                                                           \
})

/////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
/////////////////////////////////////////////////////////////////////////////////////////////////////

extern char obj_log_buffer[256];
extern int  indentation;

/////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions
/////////////////////////////////////////////////////////////////////////////////////////////////////

int  obj_log(const ae_obj_t * const obj, char * desc);
void indent(void);
void outdent(void);
