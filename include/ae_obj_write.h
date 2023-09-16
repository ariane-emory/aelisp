#pragma once

#include <stdbool.h>

#include <ae_obj_pool.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods writing / printing methods
////////////////////////////////////////////////////////////////////////////////////////////////////
void          ae_obj_write         (const ae_obj_t *  const this                                  );
void          ae_obj_put           (const ae_obj_t *  const this                                  );
void          ae_obj_put_bytes     (const ae_obj_t *  const this                                  );
char *        ae_obj_swrite        (const ae_obj_t *  const this                                  );
char *        ae_obj_sput          (const ae_obj_t *  const this                                  );
char *        ae_obj_sput_bytes    (const ae_obj_t *  const this                                  );
void          ae_obj_fwrite        (const ae_obj_t *  const this,  FILE *            stream       );
void          ae_obj_fput          (const ae_obj_t *  const this,  FILE *            stream       );
void          ae_obj_fput_bytes    (const ae_obj_t *  const this,  FILE *            stream       );
////////////////////////////////////////////////////////////////////////////////////////////////////

