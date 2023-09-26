#include "ae_util.h"

char obj_log_buffer[64];

#ifdef OLD_LOG
void obj_log(const ae_obj_t * const obj, char * desc) {
  NL;
  PR("%-24s %018p", desc, obj);
  SPC;
  ae_put(obj);
  SPC;
  ae_write(obj);
  fflush(stdout);
}
#else
void obj_log(const ae_obj_t * const obj, char * desc) {
  NL;
  PR("%-24s %018p [ %s ", desc, obj, TYPE_STR(GET_TYPE(obj)));
  ae_write(obj);
  PR(" ]");
  fflush(stdout);
}
#endif
