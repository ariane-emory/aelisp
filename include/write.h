#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PUT(this)               (ae_put(this))
#define FPUT(this, stream)      (ae_fput((this), (stream)))
#define SPUT(this)              (ae_sput((this)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PRINC(this)             (ae_princ(this))
#define FPRINC(this, stream)    (ae_fprinc((this), (stream)))
#define SPRINC(this)            (ae_sprinc(this))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PRINT(this)             (ae_print(this))
#define FPRINT(this, stream)    (ae_fprint((this), (stream)))
#define SPRINT(this)            (ae_sprint(this))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define WRITE(this)             (ae_write(this))
#define FWRITE(this, stream)    (ae_fwrite((this), (stream)))
#define SWRITE(this)            (ae_swrite(this))
////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods writing / printing methods
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_princ             (const ae_obj_t *  const this                                  );
char *        ae_sprinc            (const ae_obj_t *  const this                                  );
int           ae_fprinc            (const ae_obj_t *  const this,  FILE * stream                  );
////////////////////////////////////////////////////////////////////////////////////////////////////
int           ae_print             (const ae_obj_t *  const this                                  );
char *        ae_sprint            (const ae_obj_t *  const this                                  );
int           ae_fprint            (const ae_obj_t *  const this,  FILE * stream                  );
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

