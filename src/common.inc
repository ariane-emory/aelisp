#include "obj.h"
#include "env.h"
#include "eval.h"
#include "free_list.h"
#include "write.h"
#include "util.h"

//////////////////////////////////////////////////////////////////////////////
// data
//////////////////////////////////////////////////////////////////////////////

#define free_list_size (1 << 16)
static char mem[free_list_size] = { 0 };

ae_obj_t * program = NIL;

//////////////////////////////////////////////////////////////////////////////
// extern decls
//////////////////////////////////////////////////////////////////////////////

extern int yyparse (void);
extern FILE * yyin;

//////////////////////////////////////////////////////////////////////////////
// preface
//////////////////////////////////////////////////////////////////////////////

inline static void preface(void) {
  NL;
  printf("obj size:          %d.\n",    sizeof(ae_obj_t));
  printf("int size:          %d.\n",    sizeof(int));
  printf("nil is at:         %016p.\n", NIL);
  printf("t is at:           %016p.\n", TRUE);
  printf("Pool first:        %016p.\n", pool_first);
  printf("Pool last:         %016p.\n", pool_last);
  printf("Pool size:         %016p (%zu bytes).\n",
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE,
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE);
  printf("Strings pool size: %016p (%zu bytes).", free_list_size, free_list_size);
  NL;
}

