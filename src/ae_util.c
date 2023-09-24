#include "ae_util.h"

void obj_log(ae_obj_t * obj, char * desc) {
  NL;
  PR("%018p %-20s", obj, desc);
  SPC;
  ae_put(obj);
  SPC;
  ae_write(obj);
}
