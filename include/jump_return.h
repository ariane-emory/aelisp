#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENTS(n)                                                                                \
  {                                                                                                \
    for (int i = 0; i < (n); i++)                                                                  \
      OUTDENT;                                                                                     \
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN(obj)                                                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    ret = CAPTURED;                                                                                \
    goto end;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN_IF_ERRORP(obj, outdents)                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    if (ERRORP(CAPTURED)) {                                                                        \
      OUTDENTS((outdents));                                                                        \
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
    if (NILP(CAPTURED)) {                                                                          \
      OUTDENTS((outdents))                                                                         \
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
