#pragma once

#if ! (defined(NIL_IS_A_NULLPTR) || defined(NIL_IS_AN_INTERNED_SYMBOL) || defined(NIL_IS_AN_UNINTERNED_SYMBOL))
#  error "Define either NIL_IS_A_NULLPTR, NIL_IS_AN_INTERNED_SYMBOL, or NIL_IS_AN_UNINTERNED_SYMBOL."
#endif
