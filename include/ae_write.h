#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PUT(this)               (ae_put(this))
#define FPUT(this, stream)      (ae_fput((this), (stream)))
#define SPUT(this, stream)      (ae_fput((this))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PRINC(this)             (ae_princ(this))
#define FPRINC(this, stream)    (ae_fprinc((this), (stream)))
#define SPRINC(this)            (ae_sprinc(this))
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods writing / printing methods
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_princ             (const ae_obj_t *  const this                                  );
char *        ae_sprinc            (const ae_obj_t *  const this                                  );
int           ae_fprinc            (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_put               (const ae_obj_t *  const this                                  );
char *        ae_sput              (const ae_obj_t *  const this                                  );
int           ae_fput              (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_put_words         (const ae_obj_t *  const this                                  );
char *        ae_sput_words        (const ae_obj_t *  const this                                  );
int           ae_fput_words        (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////

