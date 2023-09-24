#include "ae_util.h"

void obj_log(ae_obj_t * obj, char * desc) {
  NL;
  PR("%-20s %018p ", desc, obj);
  SPC;
  ae_put(obj);
  SPC;
  ae_write(obj);
}
