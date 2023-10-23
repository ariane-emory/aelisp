#pragma once

#include "capture.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// jump-return and error bailing
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
#define RETURN(obj)                                                                                \
  ({                                                                                               \
    CAPTURE(obj);                                                                                  \
    ret = CAPTURED;                                                                                \
    goto end;                                                                                      \
  })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN_IF_ERRORP(obj)                                                          \
  ({ OUTDENT; RETURN_IF_ERRORP((ob)); })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define OUTDENT_AND_RETURN_NIL_IF_NILP(obj)                                                        \
  ({ OUTDENT; RETURN_NIL_IF_NILP((ob)); })
////////////////////////////////////////////////////////////////////////////////////////////////////
#define DOUBLE_OUTDENT_AND_RETURN_NIL_IF_NILP(obj)                                                 \
  ({ OUTDENT; OUTDENT; RETURN_NIL_IF_NILP((ob)); })
////////////////////////////////////////////////////////////////////////////////////////////////////
