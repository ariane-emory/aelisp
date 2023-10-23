#include "bestline.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>

#include "common.h"
#include "core.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "obj.h"
#include "util.h"
#include "write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////

#define HISTORY "repl_history.txt"

////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * root_env = NIL;

////////////////////////////////////////////////////////////////////////////////////////////////////

char *snip(const char * const buf) {
  // Get the last word if point is on a word.
  // This could be shorter but it's good enough for now.
  
  const char * const delimiters = " \t\n\r()";
  const char *       end        = buf + strlen(buf) - 1;

  while (end >= buf && strchr(delimiters, *end)) {
    if (isspace(*end))
      return calloc(1, sizeof(char)); 
    end--;
  }

  const char * start = end;
  while (start >= buf && !strchr(delimiters, *start))
    start--;

  start++; 

  int          length = end - start + 1;
  char * const result = calloc(length + 1, sizeof(char));
  strncpy(result, start, length);

  return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void maybe_complete(const char * buf,
                    const char * snipped,
                    const int snipped_len,
                    const ae_obj_t * const sym,
                    bestlineCompletions * lc) {

#ifdef AE_LOG_REPL
  PR("checking '%s' against '%s'\n", snipped, SYM_VAL(sym));
#endif
      
  if (! strncasecmp(snipped, SYM_VAL(sym), strlen(snipped))) {

#ifdef AE_LOG_REPL
    PR("\nsnipped_len %d\n", snipped_len);
    PR("buf     '%s'\nMatched '%s'.\n", buf, SYM_VAL(sym));
#endif
        
    char * const match      = SYM_VAL(sym);
    int          match_len  = strlen(match);
    int          prefix_len = strlen(buf) - strlen(snipped);
    int          needed_len = prefix_len + match_len + 2;

#ifdef AE_LOG_REPL
    PR("match_len  %d\n", match_len);
    PR("prefix_len %d\n", prefix_len);
    PR("needed_len %d\n", needed_len);
#endif

    char * const completed    = malloc(needed_len);
      
    strncpy(completed,              buf,   prefix_len);
    strncpy(completed + prefix_len, match, match_len);
    completed[needed_len - 2] = ' ';
    completed[needed_len - 1] = '\0';

#ifdef AE_LOG_REPL
    PR("Completed\n%s\n",  completed);
#endif
        
    bestlineAddCompletion(lc, completed);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void completion(const char * buf,
                bestlineCompletions * lc) {
  const char * snipped     = snip(buf);
  int          snipped_len = strlen(snipped);

  if (snipped_len != 0) {
    FOR_EACH(sym, symbols_list)
      if (KEYWORDP(sym))
        maybe_complete(buf, snipped, snipped_len, sym, lc);
    FOR_EACH(sym, ENV_SYMS(root_env))
      maybe_complete(buf, snipped, snipped_len, sym, lc);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

char *hints(const char * buf, const char ** ansi1, const char ** ansi2) {
  /* if (!strcmp(buf,"hello")) { */
  /*     *ansi1 = "\033[35m"; /\* magenta foreground *\/ */
  /*     *ansi2 = "\033[39m"; /\* reset foreground *\/ */
  /*     return " World"; */
  /* } */
  return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

bool load_file_cmd(const char * const line,
                   const char * const scanpat,
                   const int len) {
  char filename[len + 1];
  bool failed_to_open = false;
  
  if (sscanf(line, scanpat, filename) == 1) {
    ae_obj_t * program = load_file(filename, &failed_to_open);
    ae_obj_t * ret = EVAL(root_env, program);

    if (ERRORP(ret)) {
      NL;
      fprintf(stderr, "\nError evaluating '%s': ", filename);
      WRITE(ret);
      NL;
    }
  }
  else {
    printf("Error: Malformed load command.\n");
  }
  
  if (failed_to_open)
    fprintf(stderr, "Failed to open file '%s'.\n", filename);
  
  return failed_to_open;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void add_to_history(const char * const line) {
  bestlineHistoryAdd(line);
  bestlineHistorySave(HISTORY);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

extern bool read_error;

int main(int argc, char **argv) {

////////////////////////////////////////////////////////////////////////////////////////////////////
// setops
////////////////////////////////////////////////////////////////////////////////////////////////////

  if (! setopts(argc, argv)) {
    FPR(stderr, "ERROR: Bad opts!\n");
    exit(1);
  }

////////////////////////////////////////////////////////////////////////////////////////////////////
// setup root env
////////////////////////////////////////////////////////////////////////////////////////////////////
  
  root_env = setup_root_env();

////////////////////////////////////////////////////////////////////////////////////////////////////
// setup bestline stuff
////////////////////////////////////////////////////////////////////////////////////////////////////
  
  char *line;

  /* Set the completion callback. This will be called every time the
   * user uses the <tab> key. */
  bestlineSetCompletionCallback(completion);
  bestlineSetHintsCallback(hints);

  // We're always going to use balance mode.
  bestlineBalanceMode(1);
            
  /* Load history from file. The history file is just a plain text file
   * where entries are separated by newlines. */
  bestlineHistoryLoad(HISTORY); /* Load the history at startup */

  /* Now this is the main loop of the typical bestline-based application.
   * The call to bestline() will block as long as the user types something
   * and presses enter.
   *
   * The typed string is returned aks a malloc() allocated string by
   * bestline, so the user needs to free() it. */
    
  while((line = bestline("Æ> ")) != NULL) {
    if (! strncmp(line, ";l ", 3)) {
      add_to_history(line);
      load_file_cmd(line, ";l %255s", 255);
    }
    /* else if (! strncmp(line, "(load", 5)) { */
    /*   add_to_history(line); */
    /*   load_file_cmd(line, "(load \"%255[^\"]\")", 255); */
    /* } */
    else if (line[0] != '\0' && line[0] != ';') {
      add_to_history(line);
      program = NIL;
      parse_line(line);

      if (read_error) {
        FPR(stderr, "Unreadable line.\n");
        read_error = false;
      }
      
      ae_obj_t * ret = EVAL(root_env, program);
      printf(" ⇒ ");
      WRITE(ret);
      NL;      
    }
    else if (! strncmp(line, ";p", 2)) {
      pool_print();
      NL;
    }
    else if (! strncmp(line, ";q", 2)) {
      PR("Bye!");
      NL;
      exit(0);
    }
    else if (! strncmp(line, ";r", 2)) {
      root_env = setup_root_env();
    }
    else if (! strncmp(line, ";s", 2)) {
      WRITE(ENV_SYMS(root_env));
      NL;
    }
    else if (! strncmp(line, ";v", 2)) {
      WRITE(ENV_VALS(root_env));
      NL;
    }
    else if (line[0] == ';') {
      fputs("Unreconized command: ", stdout);
      fputs(line, stdout);
      fputs("\n", stdout);
    }
    free(line);
  }
  return 0;
}
