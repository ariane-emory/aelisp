#include <stdio.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_obj_pool.h"

////////////////////////////////////////////////////////////////////////////////
// pool 
////////////////////////////////////////////////////////////////////////////////

// struct ae_obj_t;

struct ae_obj_t pool[POOL_SIZE] = { 0 };

const struct ae_obj_t * const pool_first = &pool[0];
const struct ae_obj_t * const pool_last  = &pool[POOL_SIZE - 1];

struct ae_obj_t * pool_alloc_ae_obj() {
  for (int ix = POOL_SIZE - 1; ix >= 0; ix--) {
    struct ae_obj_t * obj = &pool[ix];

    if (obj->type != AE_FREE____)
      continue;

    obj->type = AE_INVALID_;
    
#ifdef NOISY_INIT
    fputs("Allocated        ", stdout);
    ae_obj_put(obj);
    putchar('\n');
#endif
  
    return obj;
  }
    
  fprintf(stderr, "ERROR: Pool is full.\n");

  assert(0);
  
  return 0;
}

void pool_free_ae_obj(struct ae_obj_t * const this) {
  ae_obj_init(this, AE_FREE____);
}

void pool_print(void) {
  puts("\nPrinting pool contents.");
  for (size_t ix = 0; ix < POOL_SIZE; ix++) {
    printf("# %5d: ", ix); 
    ae_obj_put(&pool[ix]);
    putchar('\n');
  }
  puts("Printed pool contents.");
}

void pool_clear(void) {
#ifdef NOISY_INIT
  puts("\nClearing pool contents.");
#endif

  for (size_t ix = 0; ix < POOL_SIZE; ix++)
    pool_free_ae_obj(&pool[ix]);

#ifdef NOISY_INIT
  puts("Cleared pool contents.");
#endif
}
