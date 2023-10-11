#include "bestline.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "obj.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "write.h"
#include "util.h"

#include "common.inc"

void completion(const char *buf, bestlineCompletions *lc) {
  /* if (buf[0] == 'h') { */
  /*     bestlineAddCompletion(lc,"hello"); */
  /*     bestlineAddCompletion(lc,"hello there"); */
  /* } */
}

char *hints(const char *buf, const char **ansi1, const char **ansi2) {
  /* if (!strcmp(buf,"hello")) { */
  /*     *ansi1 = "\033[35m"; /\* magenta foreground *\/ */
  /*     *ansi2 = "\033[39m"; /\* reset foreground *\/ */
  /*     return " World"; */
  /* } */
  return NULL;
}

extern void parse_line();

int main(int argc, char **argv) {

////////////////////////////////////////////////////////////////////////////////
// setup AE stuff  
////////////////////////////////////////////////////////////////////////////////
  
  preface();  
  ae_obj_t * root_env = setup_root_env();
  
////////////////////////////////////////////////////////////////////////////////
// setup bestline stuff
////////////////////////////////////////////////////////////////////////////////
  
  char *line;

  /* Set the completion callback. This will be called every time the
   * user uses the <tab> key. */
  bestlineSetCompletionCallback(completion);
  bestlineSetHintsCallback(hints);

  // We're always going to use balance mode.
  bestlineBalanceMode(1);
            
  /* Load history from file. The history file is just a plain text file
   * where entries are separated by newlines. */
  bestlineHistoryLoad("repl_history.txt"); /* Load the history at startup */

  /* Now this is the main loop of the typical bestline-based application.
   * The call to bestline() will block as long as the user types something
   * and presses enter.
   *
   * The typed string is returned as a malloc() allocated string by
   * bestline, so the user needs to free() it. */
    
  while((line = bestline("Æ> ")) != NULL) {
    /* Do something with the string. */
    if (line[0] != '\0' && line[0] != '/') {
      parse_line();      
      bestlineHistoryAdd(line); /* Add to the history. */
      bestlineHistorySave("repl_history.txt"); /* Save the history on disk. */
    } else if (line[0] == ':' && line[1] == 'q') {
      exit(0);
    } else if (line[0] == ':') {
      fputs("Unreconized command: ", stdout);
      fputs(line, stdout);
      fputs("\n", stdout);
    }
    free(line);
  }
  return 0;
}
