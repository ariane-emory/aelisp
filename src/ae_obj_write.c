#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_obj_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// write helpers
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NL      fputc('\n', stream)
#define BSPC    fputc('\b', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)
#define DQUO    fputc('"',  stream)
#define SQUO    fputc('\'', stream)

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_obj_fput(const ae_obj_t * const this, FILE * stream) {
  fprintf(stream, "%011p[ %s ", this, TYPE_STR(TYPE(this)));
  
  switch (TYPE(this)) {
  case AE_LPAREN__:
  case AE_RPAREN__:
  case AE_INVALID_:
  case AE_FREE____:
  case AE_INF_____:
    BSPC; break;
  case AE_CONS____:
    if      (! CAR(this))
      fputs("nil", stream);
    else if (! CDR(this))
      fprintf(stream, "%011p %-11p %2d",  CAR(this), CDR(this), 666); // LENGTH(this));
    else
      fprintf(stream, "%011p %-011p %2d", CAR(this), CDR(this), 666); // LENGTH(this));    
    break;
  case AE_SYMBOL__:
  case AE_STRING__:
  case AE_CHAR____:
  case AE_FLOAT___:
  case AE_INTEGER_:
  case AE_RATIONAL:
    FWRITE(this, stream);
    BSPC;
    break;
  default:
    LSQR;
    FWRITE(this, stream);
    BSPC;
    RSQR;
  }
  SPC;
  RSQR;

  return 0;
}

int ae_obj_put(const ae_obj_t * const this) {
  FPUT(this, stdout);

  return 0;
}

#define MEMSTREAM(buff, stream)                                                                    \
  char * buff;                                                                                     \
  size_t size;                                                                                     \
  FILE * stream = open_memstream(&buff, &size);

char * ae_obj_sput(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_obj_fput(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_bytes / put_bytes
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_obj_fput_bytes(const ae_obj_t * const this, FILE * stream) {
#define same_size_type long int
  size_t max = sizeof(ae_obj_t) / sizeof(same_size_type *);
  
  const same_size_type * start = (same_size_type *)this;

  for (size_t ix = 0; ix < max; ix++)  {
    switch (ix) {
    case 0:
      fprintf(stream, "type ");
      break;
    case 1:
      fprintf(stream, "data ");
      break;
    }
    fprintf(stream, "%016x ", start[ix]);
  }

  return 0;
}

int ae_obj_put_bytes(const ae_obj_t * const this) {
  ae_obj_fput_bytes(this, stdout);

  return 0;
}

char * ae_obj_sput_bytes(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_obj_fput_bytes(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_obj_write(const ae_obj_t * const this) {
  FWRITE(this, stdout);

  return 0;
}

static FILE * stream = NULL;
int           count  = 0;

static int ae_obj_fwrite_internal(const ae_obj_t * const this) {

#define COUNTED_FPUTC(c) count += (fputc(c, stream) == EOF ? 0 : 1)
  
  switch (TYPE(this)) {
  case AE_INF_____:
    fputs("∞", stream);
    break;
  case AE_CONS____:
    if (CONSP(this) && CAR(this) ) {
      COUNTED_FPUTC('(');

      FOR_EACH_CONST(elem, this)
        ae_obj_fwrite_internal(elem);

      COUNTED_FPUTC('\b');
      COUNTED_FPUTC(')');
    }
    else
      fputs("nil", stream);
    break;
  case AE_SYMBOL__:
    fputs(SYM_VAL(this), stream);
    break;
  case AE_STRING__:
    if (STR_VAL(this) == NULL) {
      int wrote = fputs("(null)", stream);
      for (; wrote < 40; wrote++)
        SPC;
    }
    else {
      COUNTED_FPUTC('"');
      fputs(STR_VAL(this), stream);
      COUNTED_FPUTC('"');
    }
    break;
  case AE_INTEGER_:
    count += fprintf(stream, "%d", this->int_val);
    break;
  case AE_RATIONAL:
    fprintf(stream, "%d/%d", this->numerator_val, this->denominator_val);
    break;
  case AE_FLOAT___:
    fprintf(stream, "%g", this->float_val);
    break;
  case AE_CHAR____:
  {
    char tmp[3] = { 0 };

    switch (this->char_val) {

#define escaped_char_case(displayed, unescaped)                                                    \
      case unescaped:                                                                              \
        tmp[0] = '\\';                                                                             \
        tmp[1] = displayed;                                                                        \
        break;
      FOR_EACH_ESCAPED_CHARACTER(escaped_char_case);
#undef escaped_char_case

    default:
      tmp[0] = this->char_val;
    }

    COUNTED_FPUTC('\'');
    fputs(tmp, stream);
    COUNTED_FPUTC('\'');
    
    break;
  }
  default:
    fprintf(stream, "UNPRINTABLE");
  }
  
  COUNTED_FPUTC(' ');

  return count;
}

int ae_obj_fwrite(const ae_obj_t * const this, FILE * stream_) {
  stream = stream_;
  ae_obj_fwrite_internal(this);

  return 0;
}

char * ae_obj_swrite(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_obj_fwrite(this, stream_);
  fclose(stream);

  return buff; // free this when you're done with it.
}

