#include "ae_util.h"

char obj_log_buffer[64];

void obj_log(const ae_obj_t * const obj, char * desc) {
  int written = 0;

  NL; written ++;

  written += PR("%-24s ", desc);
  written += WRITE(obj);
  while written++ < 40 SPC;
  WRITE(obj);
  fflush(stdout);
}

