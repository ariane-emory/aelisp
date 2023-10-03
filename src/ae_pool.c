#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#include "ae_pool.h"
#include "ae_obj.h"
#include "ae_util.h"
#include "ae_write.h"

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
    
    written +=  printf(" %04d: %08p ", ix, &pool[ix]);
    written +=  PUT(&pool[ix]);
    written ++; putchar(' ');
    
    while (written++ < 93) putchar(' ');
    
    WRITE(&pool[ix]);
    
    putchar(' ');

    if (! NILP(DOBJ(&pool[ix])))
      WRITE(DOBJ(&pool[ix]));
    
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
// _set_all_origins: it would be nice if this could live in the _env file.
////////////////////////////////////////////////////////////////////////////////////////////////////

void pool_set_all_origins(struct ae_obj_t * const kw) {
  /* PR("BEFORE:\n\n"); */
  /* pool_print(); */
  
  int first_allocated;

  for (first_allocated = 0; first_allocated < AE_OBJ_POOL_SIZE; first_allocated++) 
    if (! FREEP(&pool[first_allocated]))
      break;

  /* PR("first           %08x\n", pool_first); */
  /* PR("last            %08x\n", pool_last); */
  /* PR("first_allocated %d\n\n", first_allocated); */
  
  int ix = AE_OBJ_POOL_SIZE;

  while (ix --> first_allocated) {
    // PR("%4d %08x ", ix, &pool[ix]); FF;
   
    // PUT(&pool[ix]);
    DSET(&pool[ix], "origin", kw);
    // NL;
  }

  PR("\n\nAFTER:\n\n");
  pool_print();

}
