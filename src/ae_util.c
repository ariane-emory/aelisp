#include <stdbool.h>

#include "ae_util.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
////////////////////////////////////////////////////////////////////////////////////////////////////

char obj_log_buffer[256] = { 0 };
int  indentation         =   0;
int  obj_column          =   72;
int  tab_width           =   8;
int  auto_column         =   true;

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj_log
////////////////////////////////////////////////////////////////////////////////////////////////////

int obj_log(const ae_obj_t * const obj, char * desc) {
  NL;

  int written = 0;

  while (written++ < (indentation << 1)) SPC;
  
  written += PR("%s  ", desc);
  
  if (written >= obj_column && auto_column) {
    written++; SPC;

    obj_column = written;
    
    while (obj_column++ % tab_width);
  }
  else {
    while (written++ < obj_column) SPC;
  }

  while (written++ < obj_column) SPC;

  written += WRITE(obj);

  fflush(stdout);
  
  return written;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// indent
////////////////////////////////////////////////////////////////////////////////////////////////////

void indent(void) {
  indentation += 1;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// outdent
////////////////////////////////////////////////////////////////////////////////////////////////////

void outdent(void) {
  indentation    -= 1;

  if (indentation < 0) {
    FPR(stderr,
        "\nBANGED AGAINST THE LEFT MARGIN, THIS SHOULDN'T HAPPEN AND PROBABLY INDICATES "
        "A PROGRAMMER ERROR!\n");

#ifdef AE_DEADLY_MARGIN
    assert(((void)"hit the margin", 0));
#endif
    
    indentation   = 0;
  }
}
