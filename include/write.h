#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PUT(obj)                   (ae_put(obj))
#define FPUT(obj, stream)          (ae_fput((obj), (stream)))
#define SPUT(obj)                  (ae_sput((obj)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PRINC(obj)                 (ae_princ(obj))
#define FPRINC(obj, stream)        (ae_fprinc((obj), (stream)))
#define SPRINC(obj)                (ae_sprinc(obj))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define PRINT(obj)                 (ae_print(obj))
#define FPRINT(obj, stream)        (ae_fprint((obj), (stream)))
#define SPRINT(obj)                (ae_sprint(obj))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define WRITE(obj)                 (ae_write(obj))
#define FWRITE(obj, stream)        (ae_fwrite((obj), (stream)))
#define SWRITE(obj)                (ae_swrite(obj))
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

