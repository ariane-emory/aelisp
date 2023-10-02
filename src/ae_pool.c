#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#include "ae_pool.h"
#include "ae_obj.h"
#include "ae_write.h"

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
// _alloc
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
// _free
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_free_ae_obj(ae_obj_t * const this) {
  INIT(this, AE_FREE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _clear
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

////////////////////////////////////////////////////////////////////////////////////////////////////
// _print
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_print(void) {
  puts("\nPrinting pool contents.");

  bool started = false;
  
  for (size_t ix = 0; ix < AE_OBJ_POOL_SIZE; ix++) {
    int written = 0;

    if ((! started) && (! FREEP(&pool[ix])))
      started = true;
    else if (! started)
      continue;
    
    written +=  printf(" %04d: %018p ", ix, &pool[ix]);
    written +=  PUT(&pool[ix]);
    written ++; putchar(' ');
    while (written++ < 85) putchar(' ');
    // ae_put_words(&pool[ix]);
    // putchar('x');
    putchar('\n');
  }
  puts("Printed pool contents.");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _delocalize_object
////////////////////////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t * pool_delocalize_ptr(struct ae_obj_t * const ptr) {
  if (NILP(ptr))
    return (ae_obj_t *)(0xC0FFEEF00DC0FFEE);
  else if (ptr == TRUE)
    return (ae_obj_t *)(0xF00DCAFEBAADBEEF);
  return (ae_obj_t *)((uintptr_t)(ptr) - (uintptr_t)(pool_first));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _localize_object
////////////////////////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t * pool_localize_ptr(struct ae_obj_t * const ptr, ae_obj_t * const offset) {
  if (ptr == ((ae_obj_t *)(0xC0FFEEF00DC0FFEE)))
    return NIL;
  else if ((ae_obj_t *)(0xF00DCAFEBAADBEEF))
    return TRUE;
  return (ae_obj_t *)((uintptr_t)(ptr) + (uintptr_t)(offset));
}

