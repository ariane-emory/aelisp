#pragma once

#ifndef AE_OBJ_POOL_SIZE
#  define AE_OBJ_POOL_SIZE (1 << 16)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define DELOCALIZED(ptr)       ((pool_delocalize_ptr((struct ae_obj_t *)(ptr))))
#define LOCALIZED(ptr, offset) ((pool_localize_ptr  ((struct ae_obj_t *)(ptr), (struct ae_obj_t *)(offset)))) 

// These two need more thought:
/* #define DELOCALIZE(ptr)                                                                             \ */
/*   ((ptr) = DELOCALIZED ((ptr)));                                                                    \ */
/*   MARK_DELOCALIZED((ptr)); */
/* #define LOCALIZE(ptr, offset)                                                                       \ */
/*   ((ptr) = LOCALIZED   ((ptr)))                                                                     \ */
/*   MARK_DELOCALIZED((ptr)); */

////////////////////////////////////////////////////////////////////////////////////////////////////
// pool
////////////////////////////////////////////////////////////////////////////////////////////////////

extern const struct ae_obj_t * const pool_first;
extern const struct ae_obj_t * const pool_last;

struct ae_obj_t * pool_alloc_ae_obj      (void);
void              pool_free_ae_obj       (struct ae_obj_t * const this);
void              pool_print             (void);
void              pool_clear             (void);
struct ae_obj_t * pool_delocalize_ptr    (struct ae_obj_t * const ptr);
struct ae_obj_t * pool_localize_ptr      (struct ae_obj_t * const ptr, struct ae_obj_t * const offset);
void              pool_dset_all_allocated(struct ae_obj_t * const key, struct ae_obj_t * const value);
struct ae_obj_t * pool_get_object        (int const index);
