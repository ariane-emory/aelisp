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
    for (int ix = 1; ix < (n); ix++) {                                                             \
      printf("OUTDENT!\n");                                                                        \
      OUTDENT;                                                                                     \
    }                                                                                              \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN(obj)                                                                                \
  {                                                                                                \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (local_indents != 0)                                                                        \
      PR("RETURN at %s:%d\n", __FILE__, __LINE__);                                                 \
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
      if (local_indents != 0)                                                                      \
        PR("RETURN because ERRORP at %s:%d\n", __FILE__, __LINE__);                                \
                                                                                                   \
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
      if (local_indents != 0)                                                                      \
        PR("RETURN because NILP at %s:%d\n", __FILE__, __LINE__);                                  \
                                                                                                   \
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
