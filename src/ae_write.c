#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_write.h"

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

int ae_fput(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);
  
  int written = fprintf(stream, "[ ", this);

  written    += fprintf(stream, "%s ", TYPE_STR(GET_TYPE(this)));

  while (written++ <= 13) SPC;

  switch (GET_TYPE(this)) {
  case AE_CONS:
    written  += fprintf(stream, "%018p %018p %18d", CAR(this), CDR(this), LENGTH(this));
    break;
  case AE_ENV:
    written  += fprintf(stream, "%018p %018p %018p", this->parent, this->symbols, this->values);
    break;
  case AE_CORE_FUN:
    written  += fprintf(stream, "% -18s %-18s %018p", NAME_VAL(this), (SPECIAL_FUNP(this) ? "special" : "-"), FUN_VAL(this));
    break;
  default:
    written  += FWRITE(this, stream);
  }
  
  while (written++ <= 70) SPC;

  SPC;
  RSQR;

  written    += 2;
  
  return written;
}

int ae_put(const ae_obj_t * const this) {
  return FPUT(this, stdout);
}

#define MEMSTREAM(buff, stream)                                                                    \
  char * buff;                                                                                     \
  size_t size;                                                                                     \
  FILE * stream = open_memstream(&buff, &size);

char * ae_sput(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_fput(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_words / put_words
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_fput_words(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);
 
  int                         written = 0;
  const unsigned char * const start   = (unsigned char *)this;
  
  // This assumes the system is little-endian and renders the values as big-endian.
  
  for (unsigned int ix = 0; ix < sizeof(*this); ix++) {
    if (ix % 8 == 0)
      written += fputs("0x", stream);
    
    written += fprintf(stream, "%02x", start[(7 - (ix % 8)) + (ix & ~0x7)]);

    if ((ix + 1) % 8 == 0) {
      written += fputs(" ", stream);
    }
  }

  return written;
}

int ae_put_words(const ae_obj_t * const this) {
  return ae_fput_words(this, stdout);
}

char * ae_sput_words(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_fput_words(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_write(const ae_obj_t * const this) {
  return FWRITE(this, stdout);
}

static FILE * fwrite_stream   = NULL;
static int    fwrite_counter  = 0;

#define COUNTED_FPUTC(c, stream)     fwrite_counter += (fputc((c), (stream)) == EOF ? 0 : 1)
#define COUNTED_FPUTS(s, stream)     fwrite_counter += (fputs((s), (stream)))
#define COUNTED_FPRINTF(stream, ...) fwrite_counter += (fprintf((stream), __VA_ARGS__))

static int ae_fwrite_internal(const ae_obj_t * const this) {
  switch (GET_TYPE(this)) {
  case AE_ENV:
    COUNTED_FPRINTF(fwrite_stream, "env<%018p>", ENV_PARENT(this));
    break;
  case AE_INF:
    COUNTED_FPUTS("∞", fwrite_stream);
    break;
  case AE_CONS:
    COUNTED_FPUTC('(', fwrite_stream);

    FOR_EACH_CONST(elem, this) {
      ae_fwrite_internal(elem);
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

int ae_fwrite(const ae_obj_t * const this, FILE * stream_) {
  ASSERT_NOT_NULLP(this);

  fwrite_counter = 0;
  fwrite_stream  = stream_;

  return ae_fwrite_internal(this);
}

char * ae_swrite(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_fwrite(this, stream_);
  fclose(fwrite_stream);

  return buff; // free this when you're done with it.
}

