#pragma once

////////////////////////////////////////////////////////////////////////////////////////////////////
// pool
////////////////////////////////////////////////////////////////////////////////////////////////////

extern const struct ae_obj_t * const pool_first;
extern const struct ae_obj_t * const pool_last;

struct ae_obj_t * pool_alloc_ae_obj();
void              pool_free_ae_obj(struct ae_obj_t * const this);
void              pool_print(void);
void              pool_clear(void);
struct ae_obj_t * pool_delocate_object(struct ae_obj_t * const this);
