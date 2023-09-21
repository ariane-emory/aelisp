#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PUT(this)               (ae_obj_put(this))
#define FPUT(this, stream)      (ae_obj_fput((this), (stream)))
#define SPUT(this, stream)      (ae_obj_fput((this))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define WRITE(this)             (ae_obj_write(this))
#define FWRITE(this, stream)    (ae_obj_fwrite((this), (stream)))
#define SWRITE(this)            (ae_obj_swrite(this))
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods writing / printing methods
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_obj_write         (const ae_obj_t *  const this                                  );
char *        ae_obj_swrite        (const ae_obj_t *  const this                                  );
int           ae_obj_fwrite        (const ae_obj_t *  const this,  FILE *            stream       );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_obj_put           (const ae_obj_t *  const this                                  );
char *        ae_obj_sput          (const ae_obj_t *  const this                                  );
int           ae_obj_fput          (const ae_obj_t *  const this,  FILE *            stream       );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_obj_put_words     (const ae_obj_t *  const this                                  );
char *        ae_obj_sput_words    (const ae_obj_t *  const this                                  );
int           ae_obj_fput_words    (const ae_obj_t *  const this,  FILE *            stream       );
////////////////////////////////////////////////////////////////////////////////////////////////////

