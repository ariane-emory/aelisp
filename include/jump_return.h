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
#define RETURN_IF_ERRORP(obj, outdents)                                                            \
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
#define RETURN_NIL_IF_NILP(obj, outdents)                                                          \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    if (NILP(CAPTURED)) {                                                                          \
      OUTDENTS((outdents))                                                                         \
                                                                                                   \
      ret = CAPTURED;                                                                              \
                                                                                                   \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
