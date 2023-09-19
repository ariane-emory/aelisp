#pragma once

#if ! (defined(NIL_IS_IMPLICIT) || defined(NIL_IS_AN_UNINTERNED_SYMBOL))
#  error "Define either NIL_IS_IMPLICIT or NIL_IS_AN_UNINTERNED_SYMBOL."
#endif

#ifdef   NIL_IS_IMPLICIT
#  define NIL_DESCRIPTION "NIL_IS_IMPLICIT"
#endif

#ifdef NIL_IS_AN_UNINTERNED_SYMBOL
#  define NIL_DESCRIPTION "NIL_IS_AN_UNINTERNED_SYMBOL"
#endif

// Bookkeeping defines follow.

#if defined(NIL_IS_AN_UNINTERNED_SYMBOL)
#  define NIL_EXISTS
#endif
