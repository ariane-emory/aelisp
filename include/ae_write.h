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
#define WRITE(this)             (ae_write(this))
#define FWRITE(this, stream)    (ae_fwrite((this), (stream)))
#define SWRITE(this)            (ae_swrite(this))
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods writing / printing methods
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_write             (const ae_obj_t *  const this                                  );
char *        ae_swrite            (const ae_obj_t *  const this                                  );
int           ae_fwrite            (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_put               (const ae_obj_t *  const this                                  );
char *        ae_sput              (const ae_obj_t *  const this                                  );
int           ae_fput              (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_put_words         (const ae_obj_t *  const this                                  );
char *        ae_sput_words        (const ae_obj_t *  const this                                  );
int           ae_fput_words        (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////

