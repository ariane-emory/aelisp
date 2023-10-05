#pragma once

#include <stdbool.h>
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
#define ERR(...)         ({ FPR(stderr, __VA_ARGS__); fflush(stderr); })
#define ARRAY_SIZE(a)    (sizeof(a) / sizeof((a)[0]))
#define OLOG(o)          (obj_log((o), (#o)))

#define LOG(o, ...)                                                                                \
  ({                                                                                               \
    sprintf(log_buffer, __VA_ARGS__);                                                              \
    obj_log((o), (log_buffer));                                                                    \
  })

#define SLOG(s)                                                                                    \
  ({                                                                                               \
    NL;                                                                                            \
    int written  = 0;                                                                              \
    while (written ++ < (log_indentation << 1)) SPC;                                               \
    (fprintf(stdout, (s)));                                                                        \
  })

#define SLOGF(s, ...)                                                                              \
  ({                                                                                               \
    NL;                                                                                            \
    int written  = 0;                                                                              \
    while (written ++ < (log_indentation << 1)) SPC;                                               \
    (fprintf(stdout, (s), __VA_ARGS__));                                                           \
  })

#define LOG_RETURN_WITH_TYPE(fun_name, val)                                                        \
  ({                                                                                               \
    const char * type = GET_TYPE_STR(val);                                                         \
    /* */ char * tmp  = free_list_malloc(strlen(type) + 2);                                        \
    sprintf(tmp, ":%s", type);                                                                     \
                                                                                                   \
    LOG(val, "[%s returned a %s]", fun_name, tmp);                                                 \
                                                                                                   \
    free_list_free(tmp);                                                                           \
  })

/////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
/////////////////////////////////////////////////////////////////////////////////////////////////////

extern const int  log_column_default;
extern       int  log_column;
extern       bool log_column_auto;
extern       char log_buffer[256];
extern       int  log_indentation;
extern       int  log_tab_width;

/////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions
/////////////////////////////////////////////////////////////////////////////////////////////////////

int  obj_log(const ae_obj_t * const obj, char * desc);
void indent(void);
void outdent(void);
const char * a_or_an(const char * str);
const char * s_or_blank(int count);
