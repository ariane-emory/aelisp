#include "ae_eval.h"
#include "ae_obj.h"
#include "ae_list.h"
#include "ae_env.h"

// static const struct { uint16_t keycode; keycode_handler_fun_t handler; } keycode_handlers[] PROGMEM = {

static ae_obj_t * self(ae_obj_t * arg) {
  return arg;
}

static const struct { ae_type_t type; ae_obj_t * (*func)(ae_obj_t *); } eval_dispatch[] = {
  { AE_INTEGER, &self }
};
