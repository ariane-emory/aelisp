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
  ASSERT_NOT_NULLP(this);
  
  int total_written = fprintf(stream, "%011x[ ", this);

  int last_written = fprintf(stream, "%s ", TYPE_STR(GET_TYPE(this)));

  while (last_written++ <= 11) SPC;

  total_written += last_written;
  
  last_written = (CONSP(this))
    ? fprintf(stream, "%011p %-011p %2d", CAR(this), CDR(this), LENGTH(this))
    : FWRITE(this, stream);
  
  while (last_written++ <= 25) SPC;

  total_written += last_written;
  
  SPC;
  RSQR;

  total_written += 2;
  
  return total_written;
}

int ae_obj_put(const ae_obj_t * const this) {
  return FPUT(this, stdout);
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
// obj's fput_words / put_words
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_obj_fput_words(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);
  
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

int ae_obj_put_words(const ae_obj_t * const this) {
  ae_obj_fput_words(this, stdout);

  return 0;
}

char * ae_obj_sput_words(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_obj_fput_words(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_obj_write(const ae_obj_t * const this) {
  return FWRITE(this, stdout);
}

static FILE * fwrite_stream   = NULL;
static int    fwrite_counter  = 0;

#define COUNTED_FPUTC(c, stream)     fwrite_counter += (fputc((c), (stream)) == EOF ? 0 : 1)
#define COUNTED_FPUTS(s, stream)     fwrite_counter += (fputs((s), (stream)))
#define COUNTED_FPRINTF(stream, ...) fwrite_counter += (fprintf((stream), __VA_ARGS__))

static int ae_obj_fwrite_internal(const ae_obj_t * const this) {
  switch (GET_TYPE(this)) {
  case AE_INF:
    COUNTED_FPUTS("∞", fwrite_stream);
    break;
  case AE_CONS:
    COUNTED_FPUTC('(', fwrite_stream);

    FOR_EACH_CONST(elem, this) {
      ae_obj_fwrite_internal(elem);
      fflush(fwrite_stream);
        
      if (! NILP(CDR(position)))
        COUNTED_FPUTC(' ', fwrite_stream);
    }
      
    COUNTED_FPUTC(')', fwrite_stream);
    break;
  case AE_SYMBOL:
    COUNTED_FPUTS(SYM_VAL(this), fwrite_stream);
    break;
  case AE_STRING:
    if (STR_VAL(this) == NULL) {
      COUNTED_FPUTS("(null)", fwrite_stream);
    }
    else {
      COUNTED_FPUTC('"', fwrite_stream);
      COUNTED_FPUTS(STR_VAL(this), fwrite_stream);
      COUNTED_FPUTC('"', fwrite_stream);
    }
    break;
  case AE_INTEGER:
    COUNTED_FPRINTF(fwrite_stream, "%d", this->int_val);
    break;
  case AE_RATIONAL:
    COUNTED_FPRINTF(fwrite_stream, "%d/%d", this->numerator_val, this->denominator_val);
    break;
  case AE_FLOAT:
    COUNTED_FPRINTF(fwrite_stream, "%g", this->float_val);
    break;
  case AE_CHAR:
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

    COUNTED_FPUTC('\'', fwrite_stream);
    COUNTED_FPUTS(tmp, fwrite_stream);
    COUNTED_FPUTC('\'', fwrite_stream);
    
    break;
  }
  default:
    COUNTED_FPRINTF(fwrite_stream, "??");
  }
  
  fflush(fwrite_stream);
  
  return fwrite_counter;
}

int ae_obj_fwrite(const ae_obj_t * const this, FILE * stream_) {
  ASSERT_NOT_NULLP(this);

  fwrite_counter = 0;
  fwrite_stream  = stream_;

  return ae_obj_fwrite_internal(this);
}

char * ae_obj_swrite(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_obj_fwrite(this, stream_);
  fclose(fwrite_stream);

  return buff; // free this when you're done with it.
}

