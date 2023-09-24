#pragma once

#include <stdio.h>

#include "ae_obj.h"
#include "ae_write.h"

#define NL            (putchar('\n'))
#define SPC           (putchar(' '))
#define DOT           (putchar('.'))
#define PR(...)       (fprintf(stdout, __VA_ARGS__))
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

#define LOG(desc, o)  (obj_log(#desc, o))

void obj_log(char * desc, struct ae_obj_t * obj);
