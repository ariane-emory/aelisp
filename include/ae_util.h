#pragma once

#include <stdio.h>

#define NL      (putchar('\n'))
#define SPC     (putchar(' '))
#define DOT     (putchar('.'))
#define PR(...) (fprintf(stdout, __VA_ARGS__))
