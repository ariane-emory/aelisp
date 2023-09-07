#include "ae_type.h"

#define return_str(x) case x: return #x;
const char * const ae_type_str(const ae_type_t this) {
  switch (this) {
    FOR_LEXED_TYPES_DO(return_str);
  default: return "UNRECOGNIZED!";
  }
}
#undef return_str
