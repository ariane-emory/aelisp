#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#include "pool.h"
#include "obj.h"
#include "util.h"
#include "write.h"

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
  
  for (long long int ix = 0; ix < AE_OBJ_POOL_SIZE; ix++) {
    int written = 0;

    if ((! started) && (! FREEP(&pool[ix])))
      started = true;
    else if (! started)
      continue;
    
    written +=  printf(" %04d: %08p ", ix, &pool[ix]);
    written +=  PUT(&pool[ix]);
    written ++; SPC;

    FF;
    
    while (written++ < 72) SPC;
    
    WRITE(&pool[ix]);
    
    SPC; SPC;

    FF;
    
#ifdef AE_DEBUG_OBJ
    if (! NILP(DOBJ(&pool[ix])))
      WRITE(DOBJ(&pool[ix]));
#endif
    
    putchar('\n');
  }
  
  puts("Printed pool contents.");
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _delocalize_ptr
////////////////////////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t * pool_delocalize_ptr(struct ae_obj_t * const ptr) {
  if (NILP(ptr))
    return (ae_obj_t *)(0xC0FFEEF00DC0FFEE);
  else if (ptr == TRUE)
    return (ae_obj_t *)(0xF00DCAFEBAADBEEF);

  ae_obj_t * ret = (ae_obj_t *)((uintptr_t)(ptr) - (uintptr_t)(pool_first));

  // PR("Subtracting %016p from %016p = %016p.\n", pool_first, ptr, ret);
    
  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _localize_ptr
////////////////////////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t * pool_localize_ptr(struct ae_obj_t * const ptr, ae_obj_t * const offset) {
  if      (ptr == (ae_obj_t *)(0xC0FFEEF00DC0FFEE))
    return NIL;
  else if (ptr == (ae_obj_t *)(0xF00DCAFEBAADBEEF))
    return TRUE;

  ae_obj_t * ret = (ae_obj_t *)((uintptr_t)(ptr) + (uintptr_t)(offset));

  // PR("Adding      %016p to   %016p = %016p.\n", offset, ptr, ret);
  
  return ret;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _dset_all_allocated
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_dset_all_allocated(struct ae_obj_t * const key, struct ae_obj_t * const value) {
#ifndef AE_DEBUG_OBJ
  (void)key;
  (void)value;
#else
  int first_allocated;

  for (first_allocated = 0; first_allocated < AE_OBJ_POOL_SIZE; first_allocated++) 
    if (! FREEP(&pool[first_allocated]))
      break;

#ifdef AE_SHARED_PRIMORDIAL_TAIL
  static ae_obj_t * common_tail = NIL;
  KSET(common_tail, key, value); 
#endif
  
  int ix = AE_OBJ_POOL_SIZE;
  
  while (ix --> first_allocated) {
#ifdef AE_SHARED_PRIMORDIAL_TAIL
    DOBJ(&pool[ix]) = common_tail;
#else
    KSET(DOBJ(&pool[ix]), key, value);
#endif        
  }
#endif
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// _get_object
////////////////////////////////////////////////////////////////////////////////////////////////////

struct ae_obj_t * pool_get_object(int const index) {
  return &pool[index];
}
