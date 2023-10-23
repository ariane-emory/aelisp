#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
////////////////////////////////////////////////////////////////////////////////////////////////////
#define JUMP_RETURN_DECLS                                                                          \
  int        local_indents = 0;                                                                    \
  ae_obj_t * ret           = NIL
////////////////////////////////////////////////////////////////////////////////////////////////////
#define JUMP_RETURN_EXIT                                                                           \
  assert(local_indent == 0);                                                                       \
                                                                                                   \
  return ret;
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENTS(n)                                                                                \
  {                                                                                                \
    for (int ix = 1; ix < (n); ix++)                                                               \
      OUTDENT;                                                                                     \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN(obj)                                                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    OUTDENTS(local_indents);                                                                       \
                                                                                                   \
    ret = CAPTURED;                                                                                \
                                                                                                   \
    goto end;                                                                                      \
  })
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
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_NIL_IF_NILP(obj)                                                                    \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (NILP(CAPTURED)) {                                                                          \
      OUTDENTS(local_indents)                                                                      \
                                                                                                   \
        ret = CAPTURED;                                                                            \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
