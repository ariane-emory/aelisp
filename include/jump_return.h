#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENTS(n)                                                                                \
  {                                                                                                \
    for (int i = 0; i < n; i++)                                                                    \
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
#define RETURN_IF_ERRORP(obj)                                                                      \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    if (ERRORP(CAPTURED)) {                                                                        \
      ret = CAPTURED;                                                                              \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_NIL_IF_NILP(obj)                                                                    \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    if (NILP(CAPTURED)) {                                                                          \
      ret = CAPTURED;                                                                              \
      goto end;                                                                                    \
    }                                                                                              \
    CAPTURED;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
