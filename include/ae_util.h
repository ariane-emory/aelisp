#pragma once

#include <stdio.h>

#include "ae_obj.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NL            (putchar('\n'))
#define SPC           (putchar(' '))

#ifndef DOT
#  define DOT         (putchar('.'))
#endif

#define INDENT        (indent())
#define OUTDENT       (outdent())

#define PR(...)       (fprintf(stdout, __VA_ARGS__))
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))
#define OLOG(o)       (obj_log((o), (#o)))

#define LOG(o, ...)                                                                                 \
{                                                                                                   \
  sprintf(obj_log_buffer, __VA_ARGS__);                                                             \
  obj_log((o), (obj_log_buffer));                                                                   \
}

#define SLOG(s)                                                                                     \
{                                                                                                   \
  int written  = 0;                                                                                 \
  while (written ++ < (indentation << 1)) SPC;                                                      \
  (fprintf(stdout, s));                                                                             \
}

#define FLOG(s, ...)                                                                                \
{                                                                                                   \
  int written  = 0;                                                                                 \
  while (written ++ < (indentation << 1)) SPC;                                                      \
  (fprintf(stdout, s, __VA_ARGS__));                                                                \
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
////////////////////////////////////////////////////////////////////////////////////////////////////

extern char obj_log_buffer[64];
extern int  indentation;
////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions
////////////////////////////////////////////////////////////////////////////////////////////////////

void obj_log(const ae_obj_t * const obj, char * desc);
void indent(void);
void outdent(void);
