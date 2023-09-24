#pragma once

#include <stdio.h>

#include "ae_obj.h"
#include "ae_write.h"

#define NL            (putchar('\n'))
#define SPC           (putchar(' '))
#define DOT           (putchar('.'))
#define PR(...)       (fprintf(stdout, __VA_ARGS__))
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

#define OLOG(o)       (obj_log((o), (#o)))

extern char obj_log_buffer[64];

void obj_log(ae_obj_t * obj, char * desc);

#define LOG(o, desc)  \
({ \
  (obj_log((o), (desc))); \
})
