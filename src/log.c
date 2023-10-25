#include <stdbool.h>
#include <time.h>

#include "log.h"

// #define US (putchar('_'))
#define US SPC

////////////////////////////////////////////////////////////////////////////////////////////////////
// Logging related data
////////////////////////////////////////////////////////////////////////////////////////////////////
const int  log_column_default =   40;
/* */ int  log_column         =   log_column_default;
/* */ bool log_column_auto    =   true;
/* */ char log_buffer[256]    = { 0 };
/* */ int  log_indentation    =   0;
/* */ int  log_tab_width      =   8;
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// obj_log
////////////////////////////////////////////////////////////////////////////////////////////////////
int obj_log(const ae_obj_t * const obj, char * desc) {
  NL;

  int written = 0;

  while (written++ < (log_indentation)) putchar(' ');
  
  written += PR("%s", desc);
  
  if (written >= log_column && log_column_auto) {
    written++; US;

    log_column = written;
    
    while (log_column++ % log_tab_width);
  }
  else {
    while (written++ < log_column) US; //SPC;
  }

  while (written++ < log_column) US; //SPC;

  written += WRITE(obj);

  fflush(stdout);
  
  return written;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// indent
////////////////////////////////////////////////////////////////////////////////////////////////////
int indent(void) {
  log_indentation += 1;
  return 1;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// outdent
////////////////////////////////////////////////////////////////////////////////////////////////////
int outdent(void) {
/*   if (log_indentation == 0) { */
/*     FPR(stderr, */
/*         "\nBANGED AGAINST THE LEFT MARGIN, THIS SHOULDN'T HAPPEN AND PROBABLY INDICATES " */
/*         "A PROGRAMMER ERROR!\n"); */

/* #ifdef AE_DEADLY_MARGIN */
/*     assert(((void)"hit the margin", 0)); */
/* #endif */
/*   } */

  log_indentation    -= 1;

  return -1;
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// a_or_an
////////////////////////////////////////////////////////////////////////////////////////////////////
const char * a_or_an(const char * str) {
  return strchr("aeiouAEIOU", str[0]) ? "an" : "a";
}
////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////
// s_or_blank
////////////////////////////////////////////////////////////////////////////////////////////////////
const char * s_or_blank(int count) {
  return count == 1 ? "" : "s";
}
////////////////////////////////////////////////////////////////////////////////////////////////////
