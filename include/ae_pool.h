#pragma once

////////////////////////////////////////////////////////////////////////////////////////////////////
// macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define DELOCALIZED(ptr)  ((pool_delocalize_ptr(((struct ae_obj_t *)ptr))))
#define LOCALIZED(ptr)    ((pool_localize_ptr(((struct ae_obj_t *)ptr))))
#define DELOCALIZE(ptr)   ((ptr) = DELOCALIZED((ptr)))
#define LOCALIZE(ptr)     ((ptr) = LOCALIZED((ptr)))

////////////////////////////////////////////////////////////////////////////////////////////////////
// pool
////////////////////////////////////////////////////////////////////////////////////////////////////

extern const struct ae_obj_t * const pool_first;
extern const struct ae_obj_t * const pool_last;

struct ae_obj_t * pool_alloc_ae_obj   (void);
void              pool_free_ae_obj    (struct ae_obj_t * const this);
void              pool_print          (void);
void              pool_clear          (void);
struct ae_obj_t * pool_delocalize_ptr (struct ae_obj_t * const ptr);
struct ae_obj_t * pool_localize_ptr   (struct ae_obj_t * const ptr);
