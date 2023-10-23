#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
////////////////////////////////////////////////////////////////////////////////////////////////////
#define JUMP_RETURN_DECLS                                                                            \
  int        local_indents = 0;                                                                    \
  ae_obj_t * ret           = NIL;
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENTS(n)                                                                                \
  {                                                                                                \
    for (int ix = 1; ix < (n); ix++)                                                               \
      OUTDENT;                                                                                     \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN(obj, outdents)                                                          \
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
#define RETURN(obj) (OUTDENT_AND_RETURN((obj), 0))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN_IF_ERRORP(obj, outdents)                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (ERRORP(CAPTURED)) {                                                                        \
      OUTDENTS(local_indents);                                                                        \
                                                                                                   \
      ret = CAPTURED;                                                                              \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_IF_ERRORP(obj) (OUTDENT_AND_RETURN_IF_ERRORP((obj), 0))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN_NIL_IF_NILP(obj, outdents)                                              \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
                                                                                                   \
    if (NILP(CAPTURED)) {                                                                          \
      OUTDENTS(local_indents)                                                                         \
                                                                                                   \
        ret = CAPTURED;                                                                            \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_NIL_IF_NILP(obj) (OUTDENT_AND_RETURN_NIL_IF_NILP((obj), 0))
////////////////////////////////////////////////////////////////////////////////////////////////////
