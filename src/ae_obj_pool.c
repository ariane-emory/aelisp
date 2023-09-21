#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_obj_pool.h"

#ifndef AE_OBJ_POOL_SIZE
#  define AE_OBJ_POOL_SIZE (1 << 10)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// pool 
////////////////////////////////////////////////////////////////////////////////////////////////////

/* */ ae_obj_t         pool[AE_OBJ_POOL_SIZE] = { 0 };
const ae_obj_t * const pool_first             = &pool[0];
const ae_obj_t * const pool_last              = &pool[AE_OBJ_POOL_SIZE - 1];

////////////////////////////////////////////////////////////////////////////////////////////////////
// alloc
////////////////////////////////////////////////////////////////////////////////////////////////////

ae_obj_t * pool_alloc_ae_obj() {
  for (int ix = AE_OBJ_POOL_SIZE - 1; ix >= 0; ix--) {

    ae_obj_t * obj = &pool[ix];

    if (! FREEP(obj))
      continue;

    SET_TYPE(obj, AE_INVALID);
    
#ifdef NOISY_INIT
    fputs("Allocated        ", stdout);
    PUT(obj);
    putchar('\n');
#endif
  
    return obj;
  }
    
  fprintf(stderr, "ERROR: Pool is full.\n");

  assert(NULL);
  
  return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// free
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_free_ae_obj(ae_obj_t * const this) {
  INIT(this, AE_FREE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// print
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_print(void) {
  puts("\nPrinting pool contents.");
  for (size_t ix = 0; ix < AE_OBJ_POOL_SIZE; ix++) {
    int written = 0;
    written +=  printf(" %04d: %018p ", ix, &pool[ix]);
    written +=  PUT(&pool[ix]);
    written ++; putchar(' ');
    while (written++ < 85) putchar(' ');
    ae_obj_put_words(&pool[ix]);
    // putchar('x');
    putchar('\n');
  }
  puts("Printed pool contents.");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// clear
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_clear(void) {
#ifdef AE_LOG_CLEAR
  puts("\nClearing pool contents.");
#endif

  for (size_t ix = 0; ix < AE_OBJ_POOL_SIZE; ix++)
    pool_free_ae_obj(&pool[ix]);

#ifdef AE_LOG_CLEAR
  puts("Cleared pool contents.");
#endif
}
