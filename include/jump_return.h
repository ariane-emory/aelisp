#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
////////////////////////////////////////////////////////////////////////////////////////////////////
#define JUMP_RETURN_ENTER                                                                          \
  int        local_indents = 0;                                                                    \
  ae_obj_t * ret           = NIL
////////////////////////////////////////////////////////////////////////////////////////////////////
#define JUMP_RETURN_EXIT                                                                           \
  if (local_indents > 0)                                                                           \
    printf("WARNING: JUMP_RETURN_EXIT with local_indents > 0: %d\n", local_indents);               \
                                                                                                   \
  assert(local_indents == 0);                                                                      \
                                                                                                   \
  return ret
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENTS(n)                                                                                \
  {                                                                                                \
    if (n > 0)                                                                                     \
      printf("OUTDENTS(%d)\n", n);                                                                 \
                                                                                                   \
    for (int ix = 0; ix < (n); ix++) {                                                             \
      printf("OUTDENT!\n");                                                                        \
      OUTDENT;                                                                                     \
    }                                                                                              \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN(obj)                                                                                \
  {                                                                                                \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    OUTDENTS(local_indents);                                                                       \
                                                                                                   \
    ret = CAPTURED;                                                                                \
                                                                                                   \
    goto end;                                                                                      \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_IF_ERRORP(obj)                                                                      \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (ERRORP(CAPTURED)) {                                                                        \
      OUTDENTS(local_indents);                                                                     \
                                                                                                   \
      ret = CAPTURED;                                                                              \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
                                                                                                   \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_NIL_IF_NILP(obj)                                                                    \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (NILP(CAPTURED)) {                                                                          \
      OUTDENTS(local_indents);                                                                     \
                                                                                                   \
      ret = CAPTURED;                                                                              \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
                                                                                                   \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
